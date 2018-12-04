library("memoise")
library("data.table")
source("R/trade.R")
source("R/result.R")

#' @export
computeSimulation <- function(Symbols = NULL, startDate = NULL, endDate = NULL, timeFrame = "1D", parametersFile = config::get()$parameters, verbose = FALSE, chartAlerts = FALSE)
{
  dir.create("result", showWarnings=FALSE)
  dir.create("datacache", showWarnings=FALSE)

  resultDF <- NULL

  parameters <- getParameters(timeFrame, "trade", parametersFile)

  if(is.null(Symbols))
    AllSymbols <- getSymbolNames()
  else
    AllSymbols <- Symbols

  for(symbol in AllSymbols)
  {
    adjustDates <- sort(unique(c(index(getDividends.db(symbol)), index(getSplits.db(symbol)))))

    if(timeFrame == "1D")
      symbol <- getSymbolsDaily(symbol, filterVol = FALSE)
    else
      symbol <- getSymbolsIntraday(symbol, timeFrame, filterVol = FALSE)

    if(is.null(symbol))
      next

    symbolIdx <- base::get(symbol)
    symbolIdx <- tail(symbolIdx, nrow(symbolIdx) - 500)

    if(nrow(symbolIdx) == 0)
      next

    timeIndex <- index(symbolIdx[paste0(startDate, "/", endDate)])

    if(length(timeIndex) == 0)
      next

    operations <- data.table()

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
      }

      profit <- NULL
      type <- "none"

      if(nrow(operations) > 0 && data.table::last(operations)$stop == FALSE)
      {
        openOps <- tail(operations, min(last(rle(operations$stop)$lengths), last(rle(operations$decision)$lengths)))
        profit <- openResult(openOps, unique(openOps$symbol), tradeDate)

        if(last(openOps$decision) == "buy")
          type <- "long"

        if(last(openOps$decision) == "sell")
          type <- "short"
      }

      tradeDecision <- trade(symbol, tradeDate, parameters = parameters, profit = profit, type = type, verbose = verbose)

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

        operations <- rbind(operations, data.frame(symbol, tradeDate, decision, stop = tradeDecision$stop, price, reason = tradeDecision$reason, stringsAsFactors = FALSE))

        addAlerts(unlist(strsplit(symbol, "[.]"))[1], tradeDate, decision, price, timeFrame)

        if(chartAlerts)
          chartSymbols(symbol, endDate=tradeDate, timeFrame=timeFrame, dev="png", suffix=paste(tradeDate, decision, sep="-"), mode = "simulation", xres = 1850, smaPeriod = ifelse(!is.null(parameters), parameters$smaPeriod, 400))
      }

      if(chartAlerts && nrow(operations) > 0 && (i == length(timeIndex)))
        chartSymbols(symbol, endDate=tradeDate, timeFrame=timeFrame, dev="png", suffix=paste(tradeDate, tradeDecision$decision, sep="-"), mode = "simulation", xres = 1850, smaPeriod = ifelse(!is.null(parameters), parameters$smaPeriod, 400))
    }

    result <- singleResult(operations, max(timeIndex))

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
