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

  for(i in length(timeIndex):1)
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
    bullBuy   <- ifelse(is.null(config$trade$bull_buy), NA, config$trade$bull_buy)
    bullSell   <- ifelse(is.null(config$trade$bull_sell), NA, config$trade$bull_sell)
    bearSell   <- ifelse(is.null(config$trade$bear_sell), NA, config$trade$bear_sell)
    bearBuy   <- ifelse(is.null(config$trade$bear_buy), NA, config$trade$bear_buy)

    parameters <- data.frame(smaPeriod, upperBand, lowerBand, upChange, downChange, lowLimit, stopLoss, stopGain, bearSell, bearBuy, bullBuy, bullSell)

    price <- meanPrice(symbol)

    tradeDecision <- trade(symbol, tradeDate, parameters = parameters, price = price)

    if(is.null(tradeDecision))
      next

    print(paste(symbol, tradeDate, tradeDecision$decision, tradeDecision$reason))

    tradeAlert <- sprintf("%s%s%s", symbol, tradeDecision$decision, tradeDecision$reason)

    if(tradeDecision$decision != "hold")
    {
      price <- as.numeric(last(Cl(base::get(symbol)[paste0("/", tradeDate)])))

      logLine <- paste(symbol, tradeDate, tradeDecision$decision, price, collapse = " ")

      writeResult(symbol, logLine, "../stream")

      alert <- tradeDecision$decision
      date  <- tradeDate
      alerts <- unique(rbind(alerts, data.frame(symbol, date, alert)))

      addAlerts(unlist(strsplit(symbol, "[.]"))[1], tradeDate, tradeDecision$decision, price, timeFrame)
    }
  }

  return(alerts)
}

#' @export
computeStream <- function(Symbols = NULL, openMarket = TRUE, timeFrames = c("5M", "10M", "15M", "30M", "1H", "1D"), updateData = TRUE)
{
  stopdtime <- "18:20:00"

  tradeAlerts <- NULL
  alertSymbols <- NULL
  lastSession <- NULL
  startTime <- Sys.time()

  indexes <- new.env(hash=T, parent=emptyenv())

  config <- config::get()

  while(TRUE)
  {
    dtime <- format(Sys.time(), "%H:%M:%S", tz="America/Sao_Paulo")

    Symbols <- getSymbolNames()

    endDate <- Sys.Date()

    sendAlerts <- FALSE

    for(symbolName in Symbols)
    {
      if(updateData)
      {
        updateIntraday(symbolName)

        if(any(timeFrames %in% c("1M", "3M", "5M", "10M", "15M", "30M", "1H")))
          updateDailyFromIntraday(symbolName)
        else
          updateDaily(symbolName)
      }

      for(timeFrame in timeFrames)
      {
        if(timeFrame == "1D")
        {
          symbol <- getSymbolsDaily(symbolName)
        }
        else
        {
          symbol <- getSymbolsIntraday(symbolName, timeFrame, updateLast = TRUE)
        }

        if(is.null(symbol) || is.null(filterBadData(symbol)))
          next

        lastIdx <- as.Date(index(xts::last(base::get(symbol))))

        if(is.null(lastSession) || lastIdx > lastSession)
          lastSession <- lastIdx

        tradeDate <- lastSession

        if(timeFrame == "1D")
        {
          alert  <- computeAlerts(symbol, tradeDate, timeFrame)

          if(!is.null(alert))
            sendAlerts <- TRUE
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

            if(!is.null(alert))
              sendAlerts <- TRUE
          }
        }

        base::rm(list = base::ls(pattern = symbol, envir = .GlobalEnv), envir = .GlobalEnv)
      }
    }

    if(sendAlerts)
    {
      alerts <- getAlerts()
      alerts <- alerts[as.Date(alerts$datetime) >= Sys.Date() - 1, ]

      chartAlerts(alerts)

      sendAlert(alerts)
    }

    if(dtime > stopdtime || (!is.null(lastSession) && lastSession < Sys.Date()))
    {
      break
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

