source("trade.R")

computeBacktest <- function(Symbols, startDate, endDate, printCharts = FALSE)
{
  Symbols <- startProbe(Symbols, FALSE, 200)
  
  alertSymbols <- NULL
  
  for(symbol in Symbols)
  {
    for(dt in seq.Date(as.Date(startDate), as.Date(endDate), by="+1 days"))
    {
      decision <- trade(symbol, dt)
      
      if(decision != "hold")
      {
        alertSymbols <- c(alertSymbols, symbol)
        
        price <- sprintf("%.2f", sum(HLC(get(symbol)[as.Date(dt)]))/3)
        logLine <- paste(symbol, as.Date(dt), decision, price, collapse = " ")
        logFile <- paste("training/",symbol,".log", sep = "")
        cat(logLine, file=logFile, sep = "\n", append=TRUE)
      }
      
      if(printCharts)
      {
        #TODO imprimir aqui levando em consideracao as decisoes previas de buy/hold/sell (ordens "virtuais")
        
        #TODO mover sabosta
        #imagePath <- sprintf("chart-history/%s", symbolName)
        #dir.create(imagePath, showWarnings=FALSE)
        #chartSymbols(symbolName, startDate=startChart, dateLimit=endChart, dev="png", path=imagePath, suffix=sprintf(format(as.Date(dt), "%Y-%m-%d")))
      }
    }
  }
  
  return(alertSymbols)
}