source("trade.R")

computeBacktest <- function(Symbols, startDate, endDate, printCharts = FALSE)
{
  Symbols <- startProbe(Symbols, FALSE, 200)
  
  tradeDays <- getQuery("select distinct date from stockprices where date >= (select now() - interval 5 year) order by date desc")[,1]
  
  alertSymbols <- NULL
  
  for(symbol in Symbols)
  {
    for(dt in seq.Date(as.Date(startDate), as.Date(endDate), by="+1 days"))
    {
      if((as.Date(dt) %in% as.Date(tradeDays)) == FALSE)
         next
      
      decision <- trade(symbol, dt)
      
      if(decision != "hold")
      {
        print(paste(symbol, as.Date(dt), decision))
        
        if(symbol %in% alertSymbols == FALSE)
        {
          alertSymbols <- c(alertSymbols, symbol)
        }
        
        price <- sprintf("%.2f", sum(HLC(get(symbol)[as.Date(dt)]))/3)
        logLine <- paste(symbol, as.Date(dt), decision, price, collapse = " ")
        logFile <- paste("training/",symbol,".log", sep = "")
        cat(logLine, file=logFile, sep = "\n", append=TRUE)
        cmdLine <- sprintf("cat training/%s.log | grep -v \"0.00\" | sort -u > training/%s.bkp && mv training/%s.bkp training/%s.log", symbol, symbol, symbol, symbol)
        system(cmdLine)
        
        if(printCharts)
        {
          chartSymbols(symbol, dateLimit=as.Date(dt), dev="png")
        }
      }
    }
  }
  
  return(alertSymbols)
}