library("memoise")
library("data.table")
source("R/trade.R")
source("R/result.R")

#' @export
computeSimulation <- function(Symbols = NULL, startDate = NULL, endDate = NULL, timeFrame = "1D")
{
  dir.create("result", showWarnings=FALSE)
  dir.create("datacache", showWarnings=FALSE)

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
  bullBuy   <- ifelse(is.null(config$trade$bull_buy), NA, config$trade$bull_buy)
  bullSell   <- ifelse(is.null(config$trade$bull_sell), NA, config$trade$bull_sell)
  bearSell   <- ifelse(is.null(config$trade$bear_sell), NA, config$trade$bear_sell)
  bearBuy   <- ifelse(is.null(config$trade$bear_buy), NA, config$trade$bear_buy)

  parameters <- data.frame(smaPeriod, upperBand, lowerBand, upChange, downChange, lowLimit, stopLoss, stopGain, bearSell, bearBuy, bullBuy, bullSell)

  if(is.null(Symbols))
    AllSymbols <- getSymbolNames()
  else
    AllSymbols <- Symbols

  for(symbol in AllSymbols)
  {
    if(timeFrame == "1D")
      symbol <- getSymbolsDaily(symbolNames = symbol)
    else
      symbol <- getSymbolsIntraday(symbol, timeFrame)

    if(is.null(symbol) || is.null(filterBadData(symbol)))
      next

    indexes <- index(base::get(symbol))
    timeIndex <- tail(indexes, length(indexes) - 500)

    if(!is.null(startDate))
      timeIndex <- timeIndex[which(timeIndex >= startDate)]

    if(!is.null(endDate))
      timeIndex <- timeIndex[which(timeIndex <= endDate)]

    if(length(timeIndex) == 0)
      next

    operations <- list()

    for(i in length(timeIndex):1)
    {
      tradeDate <- timeIndex[i]

      if(is.null(filterData(symbol, tradeDate)))
        next

      tradeDecision <- trade(symbol, tradeDate, parameters = parameters, operations = operations)

      if(is.null(tradeDecision))
        next

      alerts <- new.env(hash=T, parent=emptyenv())

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

        i <- length(operations)
        operations[[i+1]] <- logLine

        addAlerts(unlist(strsplit(symbol, "[.]"))[1], tradeDate, decision, price, timeFrame)
      }
    }

    result <- singleResult(rbindlist(operations), max(timeIndex))

    if(length(result) > 0)
    {
      print(sprintf("[%s]", symbol))
      print(parameters)
      print(result)
    }

    base::rm(list = base::ls(pattern = symbol, envir = .GlobalEnv), envir = .GlobalEnv)
  }

  return(alertSymbols)
}
