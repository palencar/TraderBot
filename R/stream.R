library("config")
source("R/result.R")
source("R/alerts.R")
source("R/dbInterface.R")

computeAlerts <- function(symbol, timeIndex, timeFrame, parameters, operations, verbose)
{
  alerts <- NULL

  print(paste("COMPUTING:", symbol, collapse = " "))

  if(length(timeIndex) == 0)
    return(NULL)

  print(timeIndex)

  for(i in 1:length(timeIndex))
  {
    tradeDate <- timeIndex[i]

    profit <- NULL
    type <- "none"

    if(nrow(operations) > 0)
    {
      if(last(operations$decision) == "buy")
        type <- "long"

      if(last(operations$decision) == "sell")
        type <- "short"

      profit <- openResult(operations, symbol, tradeDate)
    }

    tradeDecision <- trade(symbol, tradeDate, parameters = parameters, profit = profit, type = type, verbose = verbose)

    if(is.null(tradeDecision))
      next

    if(tradeDecision$decision != "hold")
    {
      print(paste(symbol, tradeDate, tradeDecision$decision, tradeDecision$reason, collapse = " "))

      price <- as.numeric(last(Cl(base::get(symbol)[paste0("/", tradeDate)])))

      alert <- data.frame(symbol = unlist(strsplit(symbol, "[.]"))[1], date = tradeDate, alert = tradeDecision$decision, price, timeFrame, stop = tradeDecision$stop)
      alerts <- unique(rbind(alerts, alert))
      addAlerts(alert)
    }
  }

  return(alerts)
}

mGetParameters <- memoise(getParameters)

#' @export
computeStream <- function(Symbols = NULL, openMarket = TRUE, timeFrames = c("5M", "10M", "15M", "30M", "1H", "1D"), updateData = TRUE, verbose = FALSE)
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
    upIntraday <- FALSE

    operations <- getOperations()

    for(symbolName in Symbols)
    {
      adjustDates <- sort(unique(c(index(getDividends.db(symbolName)), index(getSplits.db(symbolName)))))

      if(updateData)
      {
        symbolId <- updateIntraday(symbolName)

        if(any(timeFrames %in% c("1M", "3M", "5M", "10M", "15M", "30M", "1H")))
          symbolDl <- updateDailyFromIntraday(symbolName)
        else
          symbolDl <- updateDaily(symbolName)

        if(!is.null(symbolDl))
          upIntraday <- TRUE

        if(getAdjust && (!is.null(symbolId) || !is.null(symbolDl)))
          updateAdjust(symbolName)
      }

      symbOp <- operations[operations$symbol == symbolName, ]
      symbOp <- symbOp[last(symbOp$type) == "buy",] #Only long operations for now

      for(timeFrame in timeFrames)
      {
        parameters <- mGetParameters(timeFrame, "trade")

        if(timeFrame == "1D")
          symbol <- getSymbolsDaily(symbolName, adjust = c("split", "dividend"))
        else
          symbol <- getSymbolsIntraday(symbolName, timeFrame, adjust = c("split", "dividend"))

        if(is.null(symbol))
          next

        lastIdx <- as.Date(index(xts::last(base::get(symbol))))

        if(lastIdx < Sys.Date())
          next

        if(is.null(lastSession) || lastIdx > lastSession)
          lastSession <- lastIdx

        tradeDate <- lastSession

        openOps <- data.table()

        if(nrow(symbOp) > 0)
        {
          openOps <- tail(symbOp, last(rle(symbOp$type)$lengths))
          openOps <- data.table(price=openOps$price, decision = openOps$type, tradeDate = as.Date(openOps$date))
        }

        if(timeFrame == "1D")
        {
          alert  <- computeAlerts(symbol, tradeDate, timeFrame, parameters, openOps, verbose)

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

            alert  <- computeAlerts(symbol, newIdx, timeFrame, parameters, openOps, verbose)

            if(!is.null(alert))
              sendAlerts <- TRUE
          }
        }

        base::rm(list = base::ls(pattern = symbol, envir = .GlobalEnv), envir = .GlobalEnv)
      }
    }

    if(config$alert$type != "none" && sendAlerts)
    {
      alerts <- getAlerts()[order(-datetime)]
      alerts <- alerts[as.Date(alerts$datetime) >= Sys.Date() - 1, ]
      alerts <- alerts[!duplicated(alerts[,c("symbol","timeframe", "alert")]), ]

      if(nrow(alerts) > 0)
      {
        chartAlerts(alerts)
        sendAlert(alerts)
      }
    }

    if(openMarket == FALSE || upIntraday == FALSE || dtime > stopdtime || (!is.null(lastSession) && lastSession < Sys.Date()))
    {
      if(any(timeFrames %in% c("1M", "3M", "5M", "10M", "15M", "30M", "1H")))
      {
        updateDailyFromIntraday()
      }
      else
      {
        for(symbol in Symbols)
          updateDaily(symbol)
      }

      break
    }
    else
    {
      minDiff <- as.integer(difftime(Sys.time(), startTime, units='mins'))

      startTime <- Sys.time()

      if(minDiff < 10)
      {
        print(paste0("difftime (mins): ", minDiff, " waiting: ", 10 - minDiff))
        Sys.sleep(3600 - (minDiff * 10))
      }
    }

    getAdjust <- FALSE
  }
}
