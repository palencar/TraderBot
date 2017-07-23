library("memoise")
library("data.table")
source("R/trade.R")
source("R/result.R")

#' @export
computeBacktest <- function(Symbols, minSamples = 1024, timeFrame = "1D", replaceFile = FALSE)
{
  dir.create("result", showWarnings=FALSE)
  dir.create("datacache", showWarnings=FALSE)

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
  indexes <- index(base::get(symbol))
  while(n < minSamples)
  {
    map <- new.env(hash=T, parent=emptyenv())

    smaPeriod = sample(50:1000, size = 1, replace = TRUE)
    upperBand = as.numeric(formatC(runif(1, min=0.6, max=2.0), digits=2,format="f"))
    lowerBand = as.numeric(formatC(runif(1, min=-2.0, max=-0.6), digits=2,format="f"))
    upChange = as.numeric(formatC(runif(1, min=0, max=8), digits=2,format="f"))
    downChange = as.numeric(formatC(runif(1, min=-8, max=0), digits=2,format="f"))
    lowLimit = as.numeric(formatC(runif(1, min=0, max=0.8), digits=2,format="f"))
    stopLoss = as.numeric(formatC(runif(1, min=0, max=1), digits=2,format="f"))
    stopGain = as.numeric(formatC(runif(1, min=1, max=5), digits=2,format="f"))

    bearSell  = as.numeric(formatC(runif(1, min=0.0, max=0.8), digits=2,format="f"))
    bearBuy  = as.numeric(formatC(runif(1, min=0.2, max=1.0), digits=2,format="f"))
    bullBuy  = as.numeric(formatC(runif(1, min=0.0, max=0.8), digits=2,format="f"))
    bullSell  = as.numeric(formatC(runif(1, min=0.2, max=1.0), digits=2,format="f"))

    parameters <- data.frame(smaPeriod, upperBand, lowerBand, upChange, downChange, lowLimit, stopLoss, stopGain, bearSell, bearBuy, bullBuy, bullSell)

    pars <- new.env(hash=T, parent=emptyenv())

    timeIndex <- tail(indexes, length(indexes) - 500)

    n <- n + nrow(parameters) ^ ncol(parameters)

    if((n %% 10) == 0)
    {
      print(paste0(Sys.time(), " Sample: ", n))
    }

    if(length(timeIndex) == 0)
    {
      n <- minSamples
      next
    }

    for(i in 1:length(timeIndex))
    {
      tradeDate <- timeIndex[i]

      if(is.null(filterDataM(symbol, tradeDate)))
        next

      tradeDecision <- trade(symbol, tradeDate, parameters = parameters, map = map)

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

        logLine <- data.frame(symbol, tradeDate, decision, price, stringsAsFactors = FALSE)

        parStr <- paste(tradeDecision$parameters, collapse = " ")

        pars[[parStr]] <- tradeDecision$parameters

        obj <- map[[parStr]]

        if(is.null(obj))
          map[[parStr]] <- logLine
        else
          map[[parStr]] <- rbind.data.frame(obj, logLine, stringsAsFactors = FALSE)
      }
    }

    outputOp <- sprintf("result/%s%s.rds", symbol, ifelse(timeFrame == "1D", ".1D", ""))

    parList <- list()
    resList <- list()
    opList  <- list()

    i <- 0

    for(parStr in ls(map))
    {
      lines <- map[[parStr]]

      result <- singleResultM(parStr, lines)

      if(!is.null(result$output))
      {
        i <- i + 1
        parList[[i]] <- pars[[parStr]]
        resList[[i]] <- result$total
        opList[[i]]  <- cbind(pars[[parStr]], rbind(result$closedDF, result$openDF))
        print(opList[[i]])
      }
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
  }, error = function(e) return(paste0("Symbol '", symbol, "'",
                                       " caused the error: '", Sys.Date(), "'")))
}
