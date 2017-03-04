library("hashmap")
library("memoise")
library("data.table")
source("R/trade.R")
source("R/result.R")

#' @export
computeBacktest <- function(Symbols, printCharts = FALSE, minSamples = 1024, limit = Inf)
{
  dir.create("result", showWarnings=FALSE)
  dir.create("datacache", showWarnings=FALSE)

  samples = 2

  AllSymbols <- startProbe(symbolNames = Symbols, minAge=730, update=FALSE)
  AllSymbols <- sample(AllSymbols)
  AllSymbols <- filterGapM(AllSymbols)

  n <- 0

  for(symbol in AllSymbols)
  tryCatch({
  while(n <= minSamples)
  {
    map <- hashmap("", "")
    map$clear()

    smaPeriod = sample(100:500, size = samples, replace = TRUE)
    upperBand = as.numeric(formatC(runif(samples, min=-2.0, max=4), digits=2,format="f"))
    lowerBand = as.numeric(formatC(runif(samples, min=-4, max=2.0), digits=2,format="f"))
    upChange = as.numeric(formatC(runif(samples, min=0, max=8), digits=2,format="f"))
    downChange = as.numeric(formatC(runif(samples, min=-8, max=0), digits=2,format="f"))
    lowLimit = as.numeric(formatC(runif(samples, min=0, max=0.8), digits=2,format="f"))
    stopLoss = as.numeric(formatC(runif(samples, min=0, max=2), digits=2,format="f"))
    stopGain = as.numeric(formatC(runif(samples, min=1, max=5), digits=2,format="f"))
    bearish  = as.numeric(formatC(runif(samples, min=0, max=0.7), digits=2,format="f"))
    bullish  = as.numeric(formatC(runif(samples, min=0.3, max=1), digits=2,format="f"))

    parameters <- data.frame(smaPeriod, upperBand, lowerBand, upChange, downChange, lowLimit, stopLoss, stopGain,
                             bearish, bullish)

    pars <- new.env(hash=T, parent=emptyenv())

    tradeDays <- getTradeDays(symbol)

    minStart <- as.Date(first(tradeDays)) + max(smaPeriod) + 730  #Min days of data
    maxStart <- as.Date(last(tradeDays)) - 730                    #2 years

    n <- n + nrow(parameters) ^ ncol(parameters)

    if(as.integer(maxStart - minStart) < 0)
      return(NULL)

    startDate <- as.Date(minStart) + sample(1:as.integer(maxStart - minStart), 1)
    endDate <- as.Date(startDate) + 730

    tradeDays <- tradeDays[which(tradeDays >= startDate)]
    tradeDays <- tradeDays[which(tradeDays <= endDate)]

    print(sprintf("Backtest %s %s/%s", symbol, startDate, endDate))

    for(tradeDate in tradeDays)
    {
      if(is.null(filterDataM(symbol, tradeDate)))
        next

      tradeDecisions <- trade(symbol, as.Date(tradeDate), parameters = parameters, map = map)

      alerts <- new.env(hash=T, parent=emptyenv())

      for(tradeDecision in tradeDecisions)
      {
        if(tradeDecision$decision != "hold")
        {
          alert <- paste(symbol, as.Date(tradeDate), tradeDecision$decision, formatC(tradeDecision$price, digits=2,format="f"), tradeDecision$reason)

          if(is.null(alerts[[alert]]))
          {
            print(alert)
            alerts[[alert]] <- TRUE
          }

          price <- sprintf("%.2f", tradeDecision$price)
          logLine <- paste(symbol, as.Date(tradeDate), tradeDecision$decision, price, collapse = " ")

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

    outputOp <- sprintf("result/%s_%s.rds", symbol, format(Sys.time(), "%m%d%H"))

    parList <- list()
    resList <- list()
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

      opFile$parameters <- tail(rbind(opFile$parameters, rbindlist(parList)), n = limit)
      opFile$results    <- tail(rbind(opFile$results, rbindlist(resList)), n = limit)

      saveRDS(opFile, outputOp)

      print(data.frame(symbol, rbindlist(parList), rbindlist(resList)))
    }
  }
  }, error = function(e) return(paste0("Symbol '", symbol, "'",
                                       " caused the error: '", Sys.Date(), "'")))
}
