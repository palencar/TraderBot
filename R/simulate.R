library("memoise")
library("data.table")
source("R/trade.R")
source("R/result.R")

#' @export
computeSimulation <- function(Symbols = NULL, startDate = NULL, endDate = NULL, timeFrame = "1D", parametersFile =  "tradeParameters.csv", chartAlerts = FALSE)
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
    adjustDates <- sort(unique(c(index(getDividends.db(symbol)), index(getSplits.db(symbol)))))

    if(timeFrame == "1D")
      symbol <- getSymbolsDaily(symbol)
    else
      symbol <- getSymbolsIntraday(symbol, timeFrame)

    if(is.null(symbol) || is.null(filterBadData(symbol)))
      next

    symbolIdx <- base::get(symbol)
    symbolIdx <- tail(symbolIdx, nrow(symbolIdx) - 500)

    if(nrow(symbolIdx) == 0)
      next

    timeIndex <- index(symbolIdx[paste0(startDate, "/", endDate)])

    if(length(timeIndex) == 0)
      next

    operations <- list()

    for(i in 1:length(timeIndex))
    {
      tradeDate <- timeIndex[i]

      if(any(as.Date(timeIndex[[i]]) >= adjustDates))
      {
        print(paste0("Adjusting ", symbol, " ", as.Date(timeIndex[[i]])))

        adjustDates <- adjustDates[adjustDates > as.Date(timeIndex[[i]])]
        adjustLimit <- min(adjustDates-1, max(timeIndex))

        if(timeFrame == "1D")
          getSymbolsDaily(unlist(strsplit(symbol, "[.]"))[1], timeLimit = adjustLimit, adjust = c("split", "dividend"))
        else
          getSymbolsIntraday(unlist(strsplit(symbol, "[.]"))[1], timeLimit = adjustLimit, timeFrame, adjust = c("split", "dividend"))

        linearRegressionIndicator(symbol, base::get(symbol)[paste0("/", adjustLimit)], refresh = TRUE, cache = "memory")
      }

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

        if(chartAlerts)
          chartSymbols(symbol, endDate=tradeDate, dev="png", suffix=paste(tradeDate, decision, sep="-"), mode = "simulation", xres = 1850, smaPeriod = ifelse(!is.null(parameters), parameters$smaPeriod, 400))
      }

      if(chartAlerts && length(operations) > 0 && (i == length(timeIndex)))
        chartSymbols(symbol, endDate=tradeDate, dev="png", suffix=paste(tradeDate, tradeDecision$decision, sep="-"), mode = "simulation", xres = 1850, smaPeriod = ifelse(!is.null(parameters), parameters$smaPeriod, 400))
    }

    opDf <- rbindlist(operations)

    result <- singleResult(opDf, max(timeIndex))

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

  if(!is.null(finalResults$total))
    return(sort(unique(finalResults$total$name)))

  return(NULL)
}
