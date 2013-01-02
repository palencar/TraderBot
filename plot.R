plotPolyReg <- function (SymbolName, polyReg, sigma)
{
  Symbol <- get(SymbolName)
  
  periodString <- sprintf("%d days", 2*length(polyReg))
  
  chartSeries(last(Symbol, periodString), name=SymbolName)
  plot(addTA(polyReg, on=1, col=3))
  plot(addTA(polyReg+sigma, on=1, col=7))
  plot(addTA(polyReg-sigma, on=1, col=7))
}
