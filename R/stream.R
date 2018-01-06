library("config")
source("R/result.R")
source("R/alerts.R")
source("R/dbInterface.R")

computeAlerts <- function(symbol, timeIndex, timeFrame, parameters)
{
  alerts <- NULL

  print(paste0("COMPUTING: ", symbol))

  if(length(timeIndex) == 0)
    return(NULL)

  print(timeIndex)

  price <- meanPrice(symbol)

  linearRegressionIndicator(symbol, base::get(symbol))

  for(i in 1:length(timeIndex))
  {
    tradeDate <- timeIndex[i]

    tradeDecision <- trade(symbol, tradeDate, parameters = parameters, price = price)

    if(is.null(tradeDecision))
      next

    print(paste(symbol, tradeDate, tradeDecision$decision, tradeDecision$reason))

    tradeAlert <- sprintf("%s%s%s", symbol, tradeDecision$decision, tradeDecision$reason)

    if(tradeDecision$decision != "hold")
    {
      tradePrice <- as.numeric(last(Cl(base::get(symbol)[paste0("/", tradeDate)])))

      logLine <- paste(symbol, tradeDate, tradeDecision$decision, tradePrice, collapse = " ")

      writeResult(symbol, logLine, "../stream")

      alert <- tradeDecision$decision
      date  <- tradeDate
      alerts <- unique(rbind(alerts, data.frame(symbol, date, alert)))

      addAlerts(unlist(strsplit(symbol, "[.]"))[1], tradeDate, tradeDecision$decision, tradePrice, timeFrame)
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
  getAdjust <- TRUE
  startTime <- Sys.time()

  indexes <- new.env(hash=T, parent=emptyenv())

  config <- config::get()
  assign("config", config, .GlobalEnv)

  while(TRUE)
  {
    dtime <- format(Sys.time(), "%H:%M:%S", tz="America/Sao_Paulo")

    if(is.null(Symbols))
      Symbols <- getSymbolNames()

    endDate <- Sys.Date()

    sendAlerts <- FALSE

    for(symbolName in Symbols)
    {
      adjustDates <- sort(unique(c(index(getDividends.db(symbolName)), index(getSplits.db(symbolName)))))

      if(updateData)
      {
        if(getAdjust)
          updateAdjust(symbolName)

        updateIntraday(symbolName)

        if(any(timeFrames %in% c("1M", "3M", "5M", "10M", "15M", "30M", "1H")))
          updateDailyFromIntraday(symbolName)
        else
          updateDaily(symbolName)
      }

      for(timeFrame in timeFrames)
      {
        parameters <- getParameters(timeFrame, "trade")

        if(timeFrame == "1D")
          symbol <- getSymbolsDaily(symbolName, adjust = c("split", "dividend"))
        else
          symbol <- getSymbolsIntraday(symbolName, timeFrame, adjust = c("split", "dividend"), updateLast = TRUE)

        if(is.null(symbol) || is.null(filterBadData(symbol)))
          next

        lastLri <- index(xts::last(linearRegressionIndicator(symbol, base::get(symbol))))

        if(length(lastLri) > 0 && any(adjustDates > as.Date(lastLri)))
          linearRegressionIndicator(symbol, base::get(symbol), refresh = TRUE)

        lastIdx <- as.Date(index(xts::last(base::get(symbol))))

        if(lastIdx < Sys.Date())
          next

        if(is.null(lastSession) || lastIdx > lastSession)
          lastSession <- lastIdx

        tradeDate <- lastSession

        if(timeFrame == "1D")
        {
          alert  <- computeAlerts(symbol, tradeDate, timeFrame, parameters)

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

            alert  <- computeAlerts(symbol, newIdx, timeFrame, parameters)

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

      if(nrow(alerts) > 0)
      {
        chartAlerts(alerts, parameters)
        sendAlert(alerts)
      }
    }

    if(openMarket == FALSE || dtime > stopdtime || (!is.null(lastSession) && lastSession < Sys.Date()))
    {
      #update cache files before the end
      for(symbol in Symbols)
      {
        getSymbolsIntraday(symbol, updateCache = TRUE, updateLast = TRUE)

        if(any(timeFrames %in% c("1M", "3M", "5M", "10M", "15M", "30M", "1H")))
          updateDailyFromIntraday(symbol)
        else
          updateDaily(symbol)
      }

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

    getAdjust <- FALSE
  }
}
