library("config")
source("R/result.R")
source("R/alerts.R")
source("R/dbInterface.R")

computeAlerts <- function(symbol, timeIndex, timeFrame)
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
    bullMin   <- ifelse(is.null(config$trade$bull_min), NA, config$trade$bull_min)
    bullMax   <- ifelse(is.null(config$trade$bull_max), NA, config$trade$bull_max)
    bearMin   <- ifelse(is.null(config$trade$bear_min), NA, config$trade$bear_min)
    bearMin   <- ifelse(is.null(config$trade$bear_max), NA, config$trade$bear_max)

    parameters <- data.frame(smaPeriod, upperBand, lowerBand, upChange, downChange, lowLimit, stopLoss, stopGain, bearMin, bearMax, bullMin, bullMax)

    price <- meanPrice(symbol)

    tradeDecision <- trade(symbol, tradeDate, parameters = parameters, price = price)

    if(is.null(tradeDecision))
      next

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

      addAlerts(symbol, tradeDate, tradeDecision$decision, timeFrame)
    }
  }

  return(alerts)
}

#' @export
computeStream <- function(Symbols = NULL, openMarket = TRUE, timeFrame = "1D")
{
  stopdtime <- "18:20:00"
  endLoop <- FALSE

  tradeAlerts <- NULL
  alertSymbols <- NULL
  alerts <- NULL
  lastSession <- NULL
  startTime <- Sys.time()

  indexes <- new.env(hash=T, parent=emptyenv())

  config <- config::get()

  while(endLoop == FALSE)
  {
    if(timeFrame == "1D")
    {
      updateDaily()

      Symbols <- getSymbolsDaily(Symbols)
    }
    else
    {
      updateIntraday()

      Symbols <- getSymbolsIntraday(Symbols, timeFrame, updateLast = TRUE)
    }

    for(symbol in Symbols)
    {
      lastIdx <- as.Date(index(xts::last(base::get(symbol))))

      if(is.null(lastSession) || lastIdx > lastSession)
        lastSession <- lastIdx

      endDate <- Sys.Date()
      tradeDate <- lastSession

      alert <- NULL

      if(timeFrame == "1D")
      {
        alert  <- computeAlerts(symbol, as.POSIXct(tradeDate), timeFrame)
        alerts <- unique(rbind(alerts, alert))
      }
      else
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

          alert  <- computeAlerts(symbol, newIdx, timeFrame)
          alerts <- unique(rbind(alerts, alert))
        }
      }

      if(!is.null(alert))
      {
        print(sprintf("Chart [%s] [%s]: %s", alert$symbol, alert$date, alert$alert))
      }
    }

    if(length(alerts) > 0)
    {
      for(symbol in as.vector(unique(alerts$symbol)))
        chartSymbols(symbol, dev="png")

      alerts <- alerts[order(alerts[,"date"], decreasing = TRUE),]
      sendAlert(alerts[!duplicated(alerts[,c('symbol','alert')]),], timeFrame)
    }

    dtime <- format(Sys.time(), "%H:%M:%S", tz="America/Sao_Paulo")

    if(dtime > stopdtime || (!is.null(lastSession) && lastSession < Sys.Date()))
    {
      endLoop <- TRUE
    }
    else
    {
      minDiff <- as.integer(difftime(Sys.time(), startTime, units='mins'))

      startTime <- Sys.time()

      if(minDiff < 60)
      {
        print(paste0("difftime (mins): ", minDiff, " waiting: ", 60 - minDiff))
        Sys.sleep(3600 - (minDiff * 60))
      }
    }
  }
}

