source("trade.R")

computeBacktest <- function(Symbols, startDate, endDate, printCharts = FALSE)
{
  Symbols <- startProbe(symbolNames = Symbols, minAge=200, update=FALSE)
  Symbols <- filterVolume(Symbols)
  Symbols <- filterIncomplete(Symbols)
  
  tradeDays <- getQuery("select distinct date from stockprices where date >= (select now() - interval 5 year) order by date desc")[,1]
  
  alertSymbols <- NULL
  
  for(symbol in Symbols)
  {
    for(tradeDate in seq.Date(as.Date(startDate), as.Date(endDate), by="+1 days"))
    {
      if((as.Date(tradeDate) %in% as.Date(tradeDays)) == FALSE || length(as.Date(tradeDate)) == 0)
         next
      
      tradeDecision <- trade(symbol, as.Date(tradeDate))
      
      print(paste(symbol, as.Date(tradeDate), tradeDecision$decision))
      
      if(tradeDecision$decision != "hold")
      {
        if(symbol %in% alertSymbols == FALSE)
        {
          alertSymbols <- c(alertSymbols, symbol)
        }
        
        price <- sprintf("%.2f", sum(HLC(get(symbol)[as.Date(tradeDate)]))/3)
        logLine <- paste(symbol, as.Date(tradeDate), tradeDecision$decision, price, collapse = " ")
        logFile <- paste("training/",symbol,".log", sep = "")
        cat(logLine, file=logFile, sep = "\n", append=TRUE)
        cmdLine <- sprintf("cat training/%s.log | grep -v \"0.00\" | sort -u > training/%s.bkp && mv training/%s.bkp training/%s.log", symbol, symbol, symbol, symbol)
        system(cmdLine)
        
        if(printCharts)
        {
          chartSymbols(symbol, dateLimit=as.Date(tradeDate), dev="png")
        }
      }
    }
  }
  
  return(alertSymbols)
}