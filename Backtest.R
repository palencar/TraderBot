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
    for(tradeDate in seq.Date(as.Date(startDate), as.Date(endDate), by="+1 days"))
    {
      if((as.Date(tradeDate) %in% as.Date(tradeDays)) == FALSE || length(as.Date(tradeDate)) == 0 || is.null(filterData(symbol, tradeDate)))
         next

      smaPeriod = seq(10, 200, 10)
      upperBand = seq(0.0, 1.0, 0.1)
      lowerBand = seq(-0.8, -1.4, -0.1)
      upChange = seq(0.3, 1, 0.1)
      downChange = seq(-0.3, -1, -0.1)
      
      price <- simPrice(symbol, tradeDate)
      
      tradeDecisions <- trade(symbol, as.Date(tradeDate), smaPeriod = smaPeriod, upperBand = upperBand, lowerBand = lowerBand, upChange = upChange, downChange = downChange, price = price)
      
      for(tradeDecision in tradeDecisions)
      {
        if(tradeDecision$decision != "hold")
        {
          print(paste(symbol, as.Date(tradeDate), tradeDecision$decision, tradeDecision$reason))
          
          if(symbol %in% alertSymbols == FALSE)
          {
            alertSymbols <- c(alertSymbols, symbol)
          }
          
          price <- sprintf("%.2f", sum(HLC(get(symbol)[as.Date(tradeDate)]))/3)
          logLine <- paste(symbol, as.Date(tradeDate), tradeDecision$decision, price, collapse = " ")
  
          parStr <- sprintf("%03d_%1.1f_%1.1f_%1.1f_%1.1f", tradeDecision$parameters[1], tradeDecision$parameters[2], tradeDecision$parameters[3],
                            tradeDecision$parameters[4], tradeDecision$parameters[5])
          writeResult(symbol, logLine, parStr)
          
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
  }
  
  return(alertSymbols)
}
