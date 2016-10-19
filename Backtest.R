source("trade.R")
source("result.R")

computeBacktest <- function(Symbols, startDate, endDate, printCharts = FALSE)
{
  tradeDays <- getTradeDays()
  
  AllSymbols <- startProbe(symbolNames = Symbols, minAge=200, update=FALSE)
  
  alertSymbols <- NULL
  
  charts <- new.env(hash=T, parent=emptyenv())
  
  for(symbol in AllSymbols)
  {
    if(file.exists(sprintf("result/%s.rds", symbol)))
    {
      results <- readRDS(sprintf("result/%s.rds", symbol))
    }
    else
    {
      results <- new.env(hash=T, parent=emptyenv())
    }
    
    for(tradeDate in seq.Date(as.Date(startDate), as.Date(endDate), by="+1 days"))
    {
      if((as.Date(tradeDate) %in% as.Date(tradeDays)) == FALSE || length(as.Date(tradeDate)) == 0 || is.null(filterData(symbol, tradeDate)))
        next
      
      smaPeriod = sample(50:300, 5)
      upperBand = formatC(runif(4, min=0, max=2), digits=1,format="f")
      lowerBand = formatC(runif(4, min=-2, max=0), digits=1,format="f")
      upChange = formatC(runif(4, min=0, max=1), digits=1,format="f")
      downChange = formatC(runif(4, min=-1, max=0), digits=1,format="f")
      
      price <- simPrice(symbol, tradeDate)
      
      tradeDecisions <- trade(symbol, as.Date(tradeDate), smaPeriod = smaPeriod, upperBand = upperBand, lowerBand = lowerBand, upChange = upChange, downChange = downChange, price = price)
      
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
          
          parStr <- sprintf("%03d_%1.1f_%1.1f_%1.1f_%1.1f", tradeDecision$parameters[1], tradeDecision$parameters[2], tradeDecision$parameters[3],
                            tradeDecision$parameters[4], tradeDecision$parameters[5])
          
          results[[parStr]] <- c(results[[parStr]], logLine)
          
          suffix <- sprintf("sma%03d", tradeDecision$parameters[1])
          
          key <- paste(symbol, tradeDate, suffix)
          
          if(printCharts && is.null(charts[[key]]))
          {
            chartSymbols(symbol, dateLimit=as.Date(tradeDate), dev="png", suffix = suffix, smaPeriod = tradeDecision$parameters[1])
            charts[[key]] <- TRUE
          }
        }
      }
    }
    
    saveRDS(results, sprintf("result/%s.rds", symbol))
  }
  
  return(alertSymbols)
}
