source('poly-reg.R')

plotSymbol <- function(SymbolName, minDays=90, maxDays=150, sigma=1, dateLimit="")
{
  reg <- findBestCurve(SymbolName, minDays, maxDays, dateLimit)
  
  plotPolyReg(reg$name, reg$regression, sigma)
}


plotPolyReg <- function (SymbolName, polyReg, sigma, dateLimit="")
{
  if(dateLimit == "")
  {
    #buscar datas das ordens executadas
    dateLimit <- sprintf("%s::%s", format(as.Date(index(first(polyReg))) - length(polyReg), "%Y-%m-%d"), format(Sys.time(), "%Y-%m-%d"))
  }
  
  Symbol <- get(SymbolName)[dateLimit]
  
  chartSeries(Symbol, name=SymbolName)
  plot(addTA(polyReg, on=1, col=3))
  plot(addTA(polyReg+sigma, on=1, col=7))
  plot(addTA(polyReg-sigma, on=1, col=7))
}
