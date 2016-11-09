source("trade.R")
source("result.R")
library("hashmap")
library("memoise")

computeBacktest <- function(Symbols, startDate, endDate, printCharts = FALSE)
{
  tradeDays <- getTradeDays()
  tradeDays <- tradeDays[which(tradeDays >= startDate)]
  tradeDays <- tradeDays[which(tradeDays <= endDate)]
  
  AllSymbols <- startProbe(symbolNames = Symbols, minAge=200, update=FALSE)
  
  forget(singleResultM)
  
  alertSymbols <- NULL
  
  smaPeriod = sample(50:300, 6)
  upperBand = as.numeric(formatC(runif(4, min=0.5, max=3), digits=2,format="f"))
  lowerBand = as.numeric(formatC(runif(4, min=-3, max=-0.5), digits=2,format="f"))
  upChange = as.numeric(formatC(runif(2, min=0, max=2), digits=2,format="f"))
  downChange = as.numeric(formatC(runif(2, min=-2, max=0), digits=2,format="f"))
  lowLimit = as.numeric(formatC(runif(4, min=0, max=1), digits=2,format="f"))
  stopLoss = as.numeric(formatC(runif(2, min=0, max=1), digits=2,format="f"))
  stopGain = as.numeric(formatC(runif(2, min=1, max=5), digits=2,format="f"))
  
  for(symbol in AllSymbols)
  {
    map <- hashmap("1", "1")
    map$clear()
    
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
          alert <- paste(symbol, as.Date(tradeDate), tradeDecision$decision, tradeDecision$reason)
          
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
    
    output <- sprintf("result/%s.txt", symbol)
    
    for(parStr in map$keys())
    {
      result <- singleResultM(parStr, unlist(strsplit(map[[parStr]], ";")))
      
      if(!is.null(result$output))
        cat(file = output, result$output, sep = "\n", append = TRUE)
      
      if(printCharts && !is.null(result$output))
      {
        operations <- unlist(strsplit(map[[parStr]], ";"))
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
    
    forget(singleResultM)
  }
  
  return(alertSymbols)
}
