library("hashmap")
library("memoise")
library("data.table")
source("R/trade.R")
source("R/result.R")

#' @export
computeBacktest <- function(Symbols, printCharts = FALSE, minSamples = 1024, timeFrame = "1D")
{
  dir.create("result", showWarnings=FALSE)
  dir.create("datacache", showWarnings=FALSE)

  if(timeFrame == "1D")
  {
    AllSymbols <- startProbe(symbolNames = Symbols, minAge=730, update=FALSE)
    AllSymbols <- sample(AllSymbols)
    AllSymbols <- filterGapM(AllSymbols)
  }
  else
  {
    AllSymbols <- getSymbolsIntraday(Symbols, timeFrame)
  }

  empty <- TRUE
  n <- 0

  for(symbol in AllSymbols)
  tryCatch({
  indexes <- index(base::get(symbol))
  while(n <= minSamples)
  {
    map <- hashmap("", "")
    map$clear()

    smaPeriod = sample(100:500, size = 1, replace = TRUE)
    upperBand = as.numeric(formatC(runif(1, min=-2.0, max=4), digits=2,format="f"))
    lowerBand = as.numeric(formatC(runif(1, min=-4, max=2.0), digits=2,format="f"))
    upChange = as.numeric(formatC(runif(1, min=0, max=8), digits=2,format="f"))
    downChange = as.numeric(formatC(runif(1, min=-8, max=0), digits=2,format="f"))
    lowLimit = as.numeric(formatC(runif(1, min=0, max=0.8), digits=2,format="f"))
    stopLoss = as.numeric(formatC(runif(1, min=0, max=2), digits=2,format="f"))
    stopGain = as.numeric(formatC(runif(1, min=1, max=5), digits=2,format="f"))
    bearish  = as.numeric(formatC(runif(1, min=0, max=0.7), digits=2,format="f"))
    bullish  = as.numeric(formatC(runif(1, min=0.3, max=1), digits=2,format="f"))

    parameters <- data.frame(smaPeriod, upperBand, lowerBand, upChange, downChange, lowLimit, stopLoss, stopGain, bearish, bullish)

    pars <- new.env(hash=T, parent=emptyenv())

    timeIndex <- tail(indexes, length(indexes) - 730)

    n <- n + nrow(parameters) ^ ncol(parameters)

    if((n %% 10000) == 0)
    {
      print(paste0(Sys.time(), " : ", n))
    }

    if(length(timeIndex) < 730)
      next

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

          price <- sprintf("%.2f", tradeDecision$price)
          logLine <- paste(symbol, tradeDate, tradeDecision$decision, price, collapse = " ")

          parStr <- paste(tradeDecision$parameters, symbol, collapse = " ")

          pars[[parStr]] <- tradeDecision$parameters

          obj <- map[[parStr]]

          if(is.na(obj))
            map[[parStr]] <- logLine
          else
            map[[parStr]] <- paste(obj, logLine, collapse = ";", sep = ";")
        }
      }
    }

    outputOp <- sprintf("result/%s.rds", symbol)

    parList <- list()
    resList <- list()
    opList  <- list()

    i <- 0

    for(parStr in map$keys())
    {
      operations <- unlist(strsplit(map[[parStr]], ";"))
      lines <- strsplit(operations, " ")
      result <- singleResultM(parStr, lines)

      if(!is.null(result$output))
      {
        i <- i + 1
        parList[[i]] <- pars[[parStr]]
        resList[[i]] <- result$total
        opList[[i]]  <- cbind(pars[[parStr]], rbind(result$closedDF, result$openDF))
        print(opList[[i]])
      }

      if(printCharts && !is.null(result$output))
      {
        path <- sprintf("charts/%s", symbol, parStr)

        for(op in operations)
        {
          op <- unlist(strsplit(op, " "))
          date <- as.Date(op[2])
          smaPeriod = as.numeric(unlist(strsplit(parStr, " "))[1])
          chartSymbols(symbol, dateLimit=date, dev="png", path = path, suffix = date, smaPeriod = smaPeriod)
        }
      }
    }

    if(i > 0)
    {
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
  }, error = function(e) return(paste0("Symbol '", symbol, "'",
                                       " caused the error: '", Sys.Date(), "'")))
}
