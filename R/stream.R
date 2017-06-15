library("config")
source("R/result.R")
source("R/alerts.R")
source("R/dbInterface.R")

#' @export
computeStream <- function(Symbols = NULL, openMarket = TRUE, timeFrame = "1D")
{
  stopdtime <- "18:20:00"
  fsmState <- "startProbe"
  tradeAlerts <- NULL
  alertSymbols <- NULL
  alerts <- NULL

  indexes <- new.env(hash=T, parent=emptyenv())

  config <- config::get()

  computeAlerts <- function(symbol, timeIndex)
  {
    alerts <- NULL

    print(paste0("COMPUTING: ", symbol))

    if(length(timeIndex) == 0)
      return(NULL)

    print(timeIndex)

    for(i in 1:length(timeIndex))
    {
      tradeDate <- timeIndex[i]

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

        if(tradeDecision$decision != "hold")
        {
          price <- as.numeric(last(Cl(base::get(symbol)[tradeDate])))

          logLine <- paste(symbol, tradeDate, tradeDecision$decision, price, collapse = " ")

          writeResult(symbol, logLine, "../stream")

          alert <- tradeDecision$decision
          date  <- tradeDate
          alerts <- unique(rbind(alerts, data.frame(symbol, date, alert)))

          addAlerts(symbol, tradeDate, tradeDecision$decision)
        }
      }
    }

    return(alerts)
  }

  while(fsmState != "end")
  {
    print(fsmState)

    if(fsmState == "startProbe")
    {
      if(timeFrame == "1D")
      {
        AllSymbols <- startProbe(symbolNames=Symbols, minAge=720)
      }
      else
      {
        updateIntraday()
        #TODO if allready has some data, only append it
        AllSymbols <- getSymbolsIntraday(Symbols, timeFrame)
      }

      fsmState <- "computeRegressions"
    }
    else if(fsmState == "computeRegressions")
    {
      if(timeFrame == "1D")
      {
        lastSession <- lastTradingSession()
      }
      else
      {
        lastSession <- as.Date.POSIXct(sapply(last(index(base::get(AllSymbols))), max))
      }

      startDate <- lastSession + 1

      if(startDate > Sys.Date())
        startDate <- Sys.Date()

      endDate <- Sys.Date()

      if(lastSession < endDate)
        openMarket <- FALSE

      tradeDate <- lastSession

      startTime <- Sys.time()
      startDay <- Sys.Date()

      if(timeFrame == "1D")
      {
        toFilter <- setdiff(AllSymbols, Symbols)
        accepted <- filterDataM(toFilter, endDate)
        Symbols <- union(accepted, Symbols)

        for(symbol in Symbols)
        {
          alert  <- computeAlerts(symbol, as.POSIXct(tradeDate))
          alerts <- unique(rbind(alerts, alert))
        }
      }
      else
      {
        for(symbol in AllSymbols)
        {
          skipIdx <- indexes[[symbol]]
          newIdx  <- index(base::get(symbol)[paste0(endDate, "/")])

          if(!is.null(skipIdx))
          {
            newIdx <- newIdx[newIdx > skipIdx]
          }

          if(length(newIdx) > 0)
          {
            indexes[[symbol]] <- max(newIdx)

            alert  <- computeAlerts(symbol, newIdx)
            alerts <- unique(rbind(alerts, alert))
          }
        }
      }

      alertSymbols <- as.vector(unique(alerts$symbol))

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
        sendAlert(alerts[!duplicated(alerts[,c('symbol','alert')]),], timeFrame)
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

