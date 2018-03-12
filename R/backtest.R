library("memoise")
library("data.table")
source("R/trade.R")
source("R/result.R")
source("R/parameters.R")

#' @export
computeBacktest <- function(Symbols, minSamples = 100, timeFrame = "1D", replaceFile = FALSE)
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
    operations[[i]] <- list()
  }

  startIdx <- min(rbindlist(parList)$smaPeriod)+500
  endIdx  <- length(indexes)

  if(startIdx > endIdx)
    next

  for(i in startIdx:endIdx)
  {
    forgetCache()

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
        stop("Failed to fetch data")

      linearRegressionIndicator(symbol, base::get(symbol)[paste0("/", adjustLimit)], refresh = TRUE, cache = "memory")
    }

    print(paste0(Sys.time(), " : ", symbol, " : ", indexes[[i]]))

    j <- 0

    for(parameters in parList)
    {
      j <- j + 1

      if(i <= parameters$smaPeriod)
        next

      tradeDecision <- trade(symbol, indexes[i], parameters = parameters, operations = operations[[j]], memoised = TRUE)

      if(is.null(tradeDecision))
        next

      if(tradeDecision$decision != "hold")
      {
        alert <- paste(symbol, indexes[i], tradeDecision$decision, formatC(tradeDecision$price, digits=2, format="f"), tradeDecision$reason)
        print(alert)

        price <- tradeDecision$price
        decision <- tradeDecision$decision

        len <- length(operations[[j]])
        operations[[j]][[len+1]] <- data.frame(symbol, tradeDate=indexes[i], decision, price, stringsAsFactors = FALSE)
      }
    }
  }

  outputOp <- sprintf("result/%s%s.rds", symbol, ifelse(timeFrame == "1D", ".1D", ""))

  opFile <- NULL
  if(file.exists(outputOp) && length(operations) > 0)
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
    opDf <- rbindlist(operations[[i]])

    result <- singleResult(opDf, lastDay)

    if(!is.null(result$output))
    {
      opList[[i]]  <- cbind(parList[[i]], rbind(result$closedDF, result$openDF))
      print(opList[[i]])

      opFile$parameters <- rbind(opFile$parameters, parameters)
      opFile$results    <- rbind(opFile$results, result$total)
      opFile$operations <- rbind(opFile$operations, rbindlist(opList))
    }
  }

  if(!is.null(opFile))
    saveRDS(opFile, outputOp)

  print(rbindlist(opList))

  }, error = function(e)
      print(paste0("Symbol ", symbol, " ", e)))
}
