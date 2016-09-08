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
    
    for(i in seq(0, 2, 0.1))
    for(j in seq(0, -2, -0.1))
    for(symbol in Symbols)
    {
      tradeDecision <- trade(symbol, as.Date(tradeDate), i, j)
      
      if(tradeDecision$decision != "hold")
      {
        print(paste(symbol, as.Date(tradeDate), tradeDecision$decision))
        
        if(symbol %in% alertSymbols == FALSE)
        {
          alertSymbols <- c(alertSymbols, symbol)
        }
        
        price <- sprintf("%.2f", sum(HLC(get(symbol)[as.Date(tradeDate)]))/3)
        logLine <- paste(symbol, as.Date(tradeDate), tradeDecision$decision, price, collapse = " ")

        writeResult(symbol, logLine, c(sprintf("%1.1f", i), sprintf("%1.1f", j)))
        
        if(printCharts)
        {
          chartSymbols(symbol, dateLimit=as.Date(tradeDate), dev="png")
        }
      }
    }
  }
  
  return(alertSymbols)
}