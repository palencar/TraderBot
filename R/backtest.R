library("memoise")
library("data.table")
source("R/trade.R")
source("R/result.R")
source("R/parameters.R")

#' @export
computeBacktest <- function(Symbols, minSamples = 1024, timeFrame = "1D", replaceFile = FALSE)
{
  dir.create("result", showWarnings=FALSE)
  dir.create("datacache", showWarnings=FALSE)

  config <- config::get()
  assign("config", config, .GlobalEnv)

  if(timeFrame == "1D")
  {
    AllSymbols <- getSymbolsDaily(symbolNames = Symbols)
  }
  else
  {
    AllSymbols <- getSymbolsIntraday(Symbols, timeFrame)
  }

  empty <- TRUE
  n <- 0

  for(symbol in AllSymbols)
  tryCatch({
  linearRegressionIndicator(symbol, base::get(symbol))
  indexes <- index(base::get(symbol))
  while(n < minSamples)
  {
    operations <- list()

    parameters <- getParameters(timeFrame, "backtest")

    print(parameters)

    pars <- NULL

    timeIndex <- tail(indexes, length(indexes) - parameters$smaPeriod)

    n <- n + 1

    print(paste0(Sys.time(), " Sample: ", n))

    if(length(timeIndex) == 0)
    {
      n <- minSamples
      next
    }

    for(i in 1:length(timeIndex))
    {
      tradeDate <- timeIndex[i]

      tradeDecision <- trade(symbol, tradeDate, parameters = parameters, operations = operations, memoised = TRUE)

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

        pars <- tradeDecision$parameters

        i <- length(operations)
        operations[[i+1]] <- data.frame(symbol, tradeDate, decision, price, stringsAsFactors = FALSE)
      }
    }

    outputOp <- sprintf("result/%s%s.rds", symbol, ifelse(timeFrame == "1D", ".1D", ""))

    parList <- list()
    resList <- list()
    opList  <- list()

    lastDay <- max(timeIndex)
    result <- singleResultM(rbindlist(operations), lastDay)

    i <- 0

    if(!is.null(result$output))
    {
      i <- i + 1
      parList[[i]] <- pars
      resList[[i]] <- result$total
      opList[[i]]  <- cbind(pars, rbind(result$closedDF, result$openDF))
      print(opList[[i]])
    }

    if(i > 0)
    {
      if(replaceFile)
      {
        if(file.exists(outputOp))
          file.remove(outputOp)
        replaceFile <- FALSE
      }

      opFile <- NULL
      if(file.exists(outputOp))
      {
        opFile <- readRDS(outputOp)
      }

      opFile$parameters <- rbind(opFile$parameters, rbindlist(parList))
      opFile$results    <- rbind(opFile$results, rbindlist(resList))
      opFile$operations <- rbind(opFile$operations, rbindlist(opList))

      saveRDS(opFile, outputOp)

      print(rbindlist(opList))
    }
  }
  }, error = function(e)
      print(paste0("Symbol ", symbol, " ", e)))
}
