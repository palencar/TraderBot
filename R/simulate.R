library("hashmap")
library("memoise")
library("data.table")
source("R/trade.R")
source("R/result.R")

#' @export
computeSimulation <- function(Symbols = NULL, startDate, endDate, chartDev = NULL)
{
  tradeDays <- getTradeDays(Symbols)
  tradeDays <- tradeDays[which(tradeDays >= startDate)]
  tradeDays <- tradeDays[which(tradeDays <= endDate)]

  AllSymbols <- startProbe(symbolNames = Symbols, minAge=as.integer(endDate-startDate)+730, update=FALSE)

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
    map <- hashmap("1", "1")
    map$clear()

    for(tradeDate in tradeDays)
    {
      if(is.null(filterDataM(symbol, tradeDate)))
        next

      tradeDecisions <- trade(symbol, as.Date(tradeDate), parameters = parameters, map = map)

      alerts <- new.env(hash=T, parent=emptyenv())

      for(tradeDecision in tradeDecisions)
      {
        if(tradeDecision$decision != "hold")
        {
          alert <- paste(symbol, as.Date(tradeDate), tradeDecision$decision, formatC(tradeDecision$price, digits=2,format="f"), tradeDecision$reason)

          if(is.null(alerts[[alert]]))
          {
            print(alert)
            alerts[[alert]] <- TRUE
          }

          if(symbol %in% alertSymbols == FALSE)
          {
            alertSymbols <- c(alertSymbols, symbol)
          }

          price <- sprintf("%.2f", tradeDecision$price)
          logLine <- paste(symbol, as.Date(tradeDate), tradeDecision$decision, price, collapse = " ")

          parStr <- paste(tradeDecision$parameters, collapse = " ")

          obj <- map[[parStr]]

          if(is.na(obj))
            map[[parStr]] <- logLine
          else
            map[[parStr]] <- paste(obj, logLine, collapse = ";", sep = ";")

          addAlerts(symbol, as.Date(tradeDate), tradeDecision$decision)
        }
      }
    }

    for(parStr in map$keys())
    {
      operations <- unlist(strsplit(map[[parStr]], ";"))
      lines <- strsplit(operations, " ")
      result <- singleResultM(parStr, lines, last(tradeDays))

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
