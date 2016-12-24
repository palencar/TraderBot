library("hashmap")
library("memoise")
library("data.table")
source("R/trade.R")
source("R/result.R")

#' @export
computeBacktest <- function(Symbols, printCharts = FALSE)
{
  tradeDays <- getTradeDays()

  AllSymbols <- startProbe(symbolNames = Symbols, minAge=200, update=FALSE)

  forget(singleResultM)

  alertSymbols <- NULL

  dir.create("result", showWarnings=FALSE)
  dir.create("datacache", showWarnings=FALSE)

  smaPeriod = sample(50:500, 3)
  upperBand = as.numeric(formatC(runif(2, min=1.0, max=4), digits=2,format="f"))
  lowerBand = as.numeric(formatC(runif(2, min=-4, max=-1.0), digits=2,format="f"))
  upChange = as.numeric(formatC(runif(2, min=0, max=2), digits=2,format="f"))
  downChange = as.numeric(formatC(runif(2, min=-2, max=0), digits=2,format="f"))
  lowLimit = as.numeric(formatC(runif(2, min=0, max=1), digits=2,format="f"))
  stopLoss = as.numeric(formatC(runif(2, min=0, max=1), digits=2,format="f"))
  stopGain = as.numeric(formatC(runif(2, min=1, max=5), digits=2,format="f"))

  for(symbol in AllSymbols)
  {
    map <- hashmap("1", "1")
    map$clear()

    tradeDays <- getTradeDays(symbol)

    minStart <- as.Date(first(tradeDays)) + 500  #At least 500 days of data
    maxStart <- as.Date(last(tradeDays)) - 730   #2 years

    startDate <- as.Date(minStart) + sample(1:as.integer(maxStart - minStart), 1)
    endDate <- as.Date(startDate) + 730

    tradeDays <- tradeDays[which(tradeDays >= startDate)]
    tradeDays <- tradeDays[which(tradeDays <= endDate)]

    print(sprintf("Backtest %s/%s", startDate, endDate))

    for(tradeDate in tradeDays)
    {
      if(is.null(filterDataM(symbol, tradeDate)))
        next

      tradeDecisions <- trade(symbol, as.Date(tradeDate), smaPeriod = smaPeriod, upperBand = upperBand, lowerBand = lowerBand, upChange = upChange, downChange = downChange, lowLimit = lowLimit, stopLoss = stopLoss, stopGain = stopGain, map = map)

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

          if(symbol %in% alertSymbols == FALSE)
          {
            alertSymbols <- c(alertSymbols, symbol)
          }

          price <- sprintf("%.2f", tradeDecision$price)
          logLine <- paste(symbol, as.Date(tradeDate), tradeDecision$decision, price, collapse = " ")

          parStr <- sprintf("%03d %1.2f %1.2f %1.2f %1.2f %1.2f %1.2f %1.2f", tradeDecision$parameters[1], tradeDecision$parameters[2], tradeDecision$parameters[3],
                            tradeDecision$parameters[4], tradeDecision$parameters[5], tradeDecision$parameters[6], tradeDecision$parameters[7], tradeDecision$parameters[8], tradeDecision$parameters[9])

          obj <- map[[parStr]]

          if(is.na(obj))
            map[[parStr]] <- logLine
          else
            map[[parStr]] <- paste(obj, logLine, collapse = ";", sep = ";")
        }
      }
    }

    outputTx <- sprintf("result/%s.txt", symbol)
    outputOp <- sprintf("result/%s.rds", symbol)

    opList <- list()
    outList <- list()
    i <- 1

    for(parStr in map$keys())
    {
      operations <- unlist(strsplit(map[[parStr]], ";"))
      lines <- strsplit(operations, " ")
      result <- singleResultM(parStr, lines)

      if(!is.null(result$output))
      {
        cat(file = outputTx, result$output, sep = "\n", append = TRUE)
        outList[[i]] <- result$output
        opList[[i]]  <- operations
        i <- i + 1
      }

      if(printCharts && !is.null(result$output))
      {
        path <- sprintf("charts/%s %s", symbol, parStr)

        for(op in operations)
        {
          op <- unlist(strsplit(op, " "))
          date <- as.Date(op[2])
          smaPeriod = as.numeric(unlist(strsplit(parStr, " "))[1])
          chartSymbols(symbol, dateLimit=date, dev="png", path = path, suffix = date, smaPeriod = smaPeriod)
        }
      }
    }

    if(length(opList) > 0)
    {
      opFile <- NULL
      if(file.exists(outputOp))
      {
        opFile <- readRDS(outputOp)
      }

      output     <- c(opFile$output, outList)
      operations <- c(opFile$operations, opList)

      opFile$output     <- output
      opFile$operations <- operations

      saveRDS(opFile, outputOp)
    }

    forget(singleResultM)
  }

  return(alertSymbols)
}
