library("memoise")
library("data.table")
source("R/trade.R")
source("R/result.R")

#' @export
computeSimulation <- function(Symbols = NULL, startDate = NULL, endDate = NULL, timeFrame = "1D", parametersFile =  "tradeParameters.csv")
{
  dir.create("result", showWarnings=FALSE)
  dir.create("datacache", showWarnings=FALSE)

  resultDF <- NULL

  config <- config::get()
  assign("config", config, .GlobalEnv)

  parameters <- getParameters(timeFrame, "trade", parametersFile)

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

      resultDF <- rbind(resultDF, result$openDF, result$closedDF)
    }

    base::rm(list = base::ls(pattern = symbol, envir = .GlobalEnv), envir = .GlobalEnv)
  }

  total <- rbind(resultDF[resultDF$state == "closed",], resultDF[resultDF$state == "open",])

  buy_price=sum(total$buy_price)
  sell_price=sum(total$sell_price)
  profit=sell_price-buy_price
  profit_pp=profit/buy_price

  finalResults <- list()
  finalResults$total   <- total
  finalResults$summary <- data.frame(buy_price, sell_price, profit, profit_pp)
  finalResults$parameters <- parameters

  saveRDS(finalResults, paste0("datacache/simulate-", gsub(" ", "_", Sys.time()), ".rds"))

  print("Parameters")
  print(finalResults$parameters)

  print("Total:")
  print(finalResults$summary)

  return(sort(unique(finalResults$total$name)))
}
