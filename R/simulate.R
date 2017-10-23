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
  openDF <- NULL
  closedDF <- NULL

  config <- config::get()
  assign("config", config, .GlobalEnv)

  parameters <- getParameters(timeFrame, "trade")

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

        i <- length(operations)
        operations[[i+1]] <- data.frame(symbol, tradeDate, decision, price, stringsAsFactors = FALSE)

        addAlerts(unlist(strsplit(symbol, "[.]"))[1], tradeDate, decision, price, timeFrame)
      }
    }

    result <- singleResult(rbindlist(operations), max(timeIndex))

    if(length(result) > 0)
    {
      print(sprintf("[%s]", symbol))
      print(parameters)
      print(result)

      openDF <- rbind(openDF, result$openDF)
      closedDF <- rbind(closedDF, result$closedDF)
    }

    base::rm(list = base::ls(pattern = symbol, envir = .GlobalEnv), envir = .GlobalEnv)
  }

  total <- rbind(closedDF, openDF)
  saveRDS(total, paste0("datacache/simulate-", gsub(" ", "_", Sys.time()), ".rds"))

  buy_price=sum(total$buy_price)
  sell_price=sum(total$sell_price)
  proffit=sell_price-buy_price
  proffit_pp=proffit/buy_price

  print("Total:")
  print(data.frame(buy_price, sell_price, proffit, proffit_pp))

  return(alertSymbols)
}
