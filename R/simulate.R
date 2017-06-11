library("memoise")
library("data.table")
source("R/trade.R")
source("R/result.R")

#' @export
computeSimulation <- function(Symbols = NULL, startDate = NULL, endDate = NULL, timeFrame = "1D")
{
  dir.create("result", showWarnings=FALSE)
  dir.create("datacache", showWarnings=FALSE)

  if(timeFrame == "1D")
  {
    AllSymbols <- startProbe(symbolNames = Symbols, minAge=730, update=FALSE)
  }
  else
  {
    AllSymbols <- getSymbolsIntraday(Symbols, timeFrame)
  }

  forget(singleResultM)

  alertSymbols <- NULL

  config <- config::get()
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

  parameters <- data.frame(smaPeriod, upperBand, lowerBand, upChange, downChange, lowLimit, stopLoss, stopGain, bearish, bullish)

  for(symbol in AllSymbols)
  {
    indexes <- index(base::get(symbol))
    timeIndex <- tail(indexes, length(indexes) - 730)

    if(!is.null(startDate))
      timeIndex <- timeIndex[which(timeIndex >= startDate)]

    if(!is.null(endDate))
      timeIndex <- timeIndex[which(timeIndex <= endDate)]

    if(length(timeIndex) == 0)
      next

    map <- new.env(hash=T, parent=emptyenv())

    for(i in 1:length(timeIndex))
    {
      tradeDate <- timeIndex[i]

      if(is.null(filterDataM(symbol, tradeDate)))
        next

      tradeDecisions <- trade(symbol, tradeDate, parameters = parameters, map = map)

      alerts <- new.env(hash=T, parent=emptyenv())

      for(tradeDecision in tradeDecisions)
      {
        if(tradeDecision$decision != "hold")
        {
          alert <- paste(symbol, tradeDate, tradeDecision$decision, formatC(tradeDecision$price, digits=2,format="f"), tradeDecision$reason)

          if(is.null(alerts[[alert]]))
          {
            print(alert)
            alerts[[alert]] <- TRUE
          }

          if(symbol %in% alertSymbols == FALSE)
          {
            alertSymbols <- c(alertSymbols, symbol)
          }

          price <- tradeDecision$price
          decision <- tradeDecision$decision

          print(paste0("DateTime: ", tradeDate))

          logLine <- data.frame(symbol, tradeDate, decision, price, stringsAsFactors = FALSE)

          parStr <- paste(tradeDecision$parameters, collapse = " ")

          obj <- map[[parStr]]

          if(is.null(obj))
            map[[parStr]] <- logLine
          else
            map[[parStr]] <- rbind.data.frame(obj, logLine)

          addAlerts(symbol, tradeDate, tradeDecision$decision, timeFrame)
        }
      }
    }

    for(parStr in ls(map))
    {
      lines <- map[[parStr]]

      result <- singleResultM(parStr, lines, last(timeIndex))

      if(length(result) > 0)
      {
        print(sprintf("[%s] [%s]", symbol, parStr))
        print(result)
      }
    }

    forget(singleResultM)
  }

  return(alertSymbols)
}
