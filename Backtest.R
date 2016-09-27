source("trade.R")
source("result.R")

computeBacktest <- function(Symbols, startDate, endDate, printCharts = FALSE)
{
  tradeDays <- getTradeDays()
  
  AllSymbols <- startProbe(symbolNames = Symbols, minAge=200, update=FALSE)

  alertSymbols <- NULL
  
  for(tradeDate in seq.Date(as.Date(startDate), as.Date(endDate), by="+1 days"))
  {
    if((as.Date(tradeDate) %in% as.Date(tradeDays)) == FALSE || length(as.Date(tradeDate)) == 0)
       next
    
    Symbols <- filterData(AllSymbols, tradeDate)
 
    for(symbol in Symbols)
    {
      chart <- FALSE
      tradeDecisions <- trade(symbol, as.Date(tradeDate), smaPeriod = 200, upperBand = 0.5, lowerBand = -0.5)
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
  
          parStr <- "default" #sprintf("%03d_%1.1f_%1.1f", tradeDecision$parameters[1], tradeDecision$parameters[2], tradeDecision$parameters[3])
          writeResult(symbol, logLine, parStr)
          chart <- TRUE
        }
        
        if(printCharts && chart)
        {
          chartSymbols(symbol, dateLimit=as.Date(tradeDate), dev="png", path = sprintf("charts_%s", parStr))
        }
      }
    }
  }
  
  return(alertSymbols)
}
