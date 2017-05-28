source("R/result.R")
source("R/alerts.R")
source("R/dbInterface.R")

#' @export
computeStream <- function(Symbols = NULL, openMarket = TRUE)
{
  stopdtime <- "18:20:00"
  fsmState <- "startProbe"
  tradeAlerts <- NULL
  alerts <- NULL

  config <- config::get()

  while(fsmState != "end")
  {
    print(fsmState)

    if(fsmState == "startProbe")
    {
      AllSymbols <- startProbe(symbolNames=Symbols, minAge=720)

      fsmState <- "computeRegressions"
    }
    else if(fsmState == "computeRegressions")
    {
      lastSession <- lastTradingSession()
      startDate <- lastSession + 1

      if(startDate > Sys.Date())
        startDate <- Sys.Date()

      endDate <- Sys.Date()

      if(lastSession < endDate)
        openMarket <- FALSE

      tradeDate <- lastSession

      startTime <- Sys.time()
      startDay <- Sys.Date()

      toFilter <- setdiff(AllSymbols, Symbols)
      accepted <- filterDataM(toFilter, endDate)
      Symbols <- union(accepted, Symbols)

      print("COMPUTING:")
      print(Symbols)

      alertSymbols <- NULL
      alertLog <- NULL

      for(symbol in Symbols)
      {
        smaPeriod <- config$trade$sma_period
        upperBand <- config$trade$upper_band
        lowerBand <- config$trade$lower_band
        upChange  <- ifelse(is.null(config$trade$up_change), NA, config$trade$up_change)
        downChange<- ifelse(is.null(config$trade$down_change), NA, config$trade$down_change)
        lowLimit  <- ifelse(is.null(config$trade$low_limit), NA, config$trade$low_limit)
        stopLoss  <- ifelse(is.null(config$trade$stop_loss), NA, config$trade$stop_loss)
        stopGain  <- ifelse(is.null(config$trade$stop_gain), NA, config$trade$stop_gain)
        bullish   <- ifelse(is.null(config$trade$bull_min), NA, config$trade$bull_min)
        bearish   <- ifelse(is.null(config$trade$bear_min), NA, config$trade$bear_min)

        parameters <- data.frame(smaPeriod, upperBand, lowerBand, upChange, downChange, lowLimit, stopLoss, stopGain, bullish, bearish)

        price <- meanPrice(symbol)

        tradeDecisions <- trade(symbol, tradeDate, parameters = parameters, price = price)

        for(tradeDecision in tradeDecisions)
        {
          print(paste(symbol, tradeDate, tradeDecision$decision, tradeDecision$reason))

          tradeAlert <- sprintf("%s%s%s", symbol, tradeDecision$decision, tradeDecision$reason)

          if(tradeDecision$decision != "hold" && (tradeAlert %in% tradeAlerts) == FALSE)
          {
            alertSymbols <- c(alertSymbols, symbol)
            tradeAlerts <- c(tradeAlert, tradeAlerts)

            price <- sprintf("%.2f", as.numeric(lastPrice(symbol)))
            logLine <- paste(symbol, tradeDate, tradeDecision$decision, price, collapse = " ")

            writeResult(symbol, logLine, "../stream")

            alertLog <- paste(alertLog, logLine, sep = "\n")

            alert <- tradeDecision$decision
            date  <- tradeDate
            alerts <- unique(rbind(alerts, data.frame(symbol, date, alert)))

            addAlerts(symbol, tradeDate, tradeDecision$decision)
          }
        }
      }

      fsmState <- "chartAlerts"
    }
    else if(fsmState == "chartAlerts")
    {
      print(sprintf("Chart [%s]: %s", alertSymbols, startTime))

      if(length(alertSymbols) > 0)
        chartSymbols(alertSymbols, dev="png")

      fsmState <- "sendAlert"

      dtime <- format(Sys.time(), "%H:%M:%S")

      if(dtime > stopdtime)
        openMarket <- FALSE
    }
    else if(fsmState == "sendAlert")
    {
      if(length(alertSymbols) > 0)
      {
        sendAlert(alerts)
      }

      if(openMarket == FALSE)
        fsmState <- "end"
      else
        fsmState <- "startProbe"
    }
    else if(fsmState == "end")
    {
      for(symbol in Symbols)
      {
        imagePath <- "chart-history"
        imageName <- sprintf("%s/%s", imagePath, symbol)
        dir.create(imagePath, showWarnings=FALSE)
        chartSymbols(Symbols=symbol, dev="png", path=imagePath, suffix=startDay)
      }
    }
  }
}
