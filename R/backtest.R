library("memoise")
library("data.table")
source("R/trade.R")
source("R/result.R")
source("R/parameters.R")

#' @export
computeBacktest <- function(Symbols, minSamples = 100, timeFrame = "1D", replaceFile = FALSE, verbose = FALSE)
{
  dir.create("result", showWarnings=FALSE)
  dir.create("datacache", showWarnings=FALSE)

  config <- config::get()
  assign("config", config, .GlobalEnv)

  for(symbol in Symbols)
  tryCatch({
  adjustDates <- sort(unique(c(index(getDividends.db(symbol)), index(getSplits.db(symbol)))))

  if(timeFrame == "1D")
    symbol <- getSymbolsDaily(symbol, adjust = NULL, filterVol = FALSE)
  else
    symbol <- getSymbolsIntraday(symbol, timeFrame, adjust = NULL, filterVol = FALSE)

  if(is.null(symbol))
    next

  indexes <- index(base::get(symbol))

  parList <- list()
  operations <- list()

  for(i in 1:minSamples)
  {
    parList[[i]] <- getParameters(timeFrame, "backtest")
    operations[[i]] <- data.table()
  }

  startIdx <- min(rbindlist(parList)$smaPeriod)+500
  endIdx  <- length(indexes)

  if(startIdx > endIdx)
    next

  print(summary(rbindlist(parList)))

  get.symbol <- symbol

  for(i in startIdx:endIdx)
  {
    if(any(as.Date(indexes[[i]]) >= adjustDates))
    {
      print(paste0("Adjusting ", symbol, " ", as.Date(indexes[[i]])))

      adjustDates <- adjustDates[adjustDates > as.Date(indexes[[i]])]
      adjustLimit <- min(adjustDates-1, max(indexes))

      if(timeFrame == "1D")
        get.symbol <- getSymbolsDaily(unlist(strsplit(symbol, "[.]"))[1], timeLimit = adjustLimit, adjust = c("split", "dividend"))
      else
        get.symbol <- getSymbolsIntraday(unlist(strsplit(symbol, "[.]"))[1], timeLimit = adjustLimit, timeFrame, adjust = c("split", "dividend"))

      if(is.null(get.symbol))
        next
    }

    print(paste0(Sys.time(), " : ", symbol, " : ", indexes[[i]]))

    j <- 0

    for(parameters in parList)
    {
      j <- j + 1

      if(i <= parameters$smaPeriod)
        next

      profit <- NULL
      type <- "none"
      symbOp <- operations[[j]]

      if(nrow(symbOp) > 0 && data.table::last(symbOp)$stop == FALSE)
      {
        openOps <- tail(symbOp, min(last(rle(symbOp$stop)$lengths), last(rle(symbOp$decision)$lengths)))
        profit <- openResult(openOps, get.symbol, indexes[i])

        if(last(openOps$decision) == "buy")
          type <- "long"

        if(last(openOps$decision) == "sell")
          type <- "short"
      }

      tradeDecision <- trade(symbol, indexes[i], parameters = parameters, profit = profit, type = type, verbose = verbose)

      if(is.null(tradeDecision))
        next

      if(tradeDecision$decision != "hold")
      {
        alert <- paste(symbol, indexes[i], tradeDecision$decision, formatC(tradeDecision$price, digits=2, format="f"), tradeDecision$reason)
        print(alert)

        price <- tradeDecision$price
        decision <- tradeDecision$decision

        operations[[j]] <- rbind(operations[[j]], data.table(symbol, tradeDate=indexes[i], decision, stop = tradeDecision$stop, price, reason = tradeDecision$reason, stringsAsFactors = FALSE))

        if(tradeDecision$stop)
        {
          result <- singleResult(operations[[j]], indexes[i])
          print(cbind(parameters, rbind(result$closedDF, result$openDF)))
        }
      }
    }
  }

  outputOp <- sprintf("result/%s%s.rds", symbol, ifelse(timeFrame == "1D", ".1D", ""))

  opFile <- NULL
  if(file.exists(outputOp))
  {
    if(!replaceFile)
      opFile <- readRDS(outputOp)
  }

  i <- 0

  opList  <- list()

  for(parameters in parList)
  {
    i <- i + 1

    lastDay <- last(indexes)

    result <- singleResult(operations[[i]], lastDay)

    if(!is.null(result$closedDF) || !is.null(result$openDF))
    {
      totalDF <- rbind(result$closedDF, result$openDF, fill=TRUE)
      opList[[i]]  <- cbind(parList[[i]], totalDF[order(open)])
    }
  }

  opDF <- rbindlist(opList, fill = TRUE)
  print(opDF)
  if(nrow(opDF) > 0)
    saveRDS(rbind(opFile, opDF, fill=TRUE), outputOp)

  }, error = function(e)
      print(paste0("Symbol ", symbol, " ", e)))
}
