source("startProbe.R")

getMeanPrice <- function(Symbol, SymbolName)
{
  mePrice <- meanPrice(SymbolName)
  
  if(is.na(mePrice))
    return(null)
    
  mPrice = xts(rep(mePrice,length(index(Symbol))), index(Symbol))
  
  col <- 3
  if(as.double(last(Cl(Symbol))) < mePrice)
    col <- 2
  
  return(sprintf("addTA(mPrice, lwd=1, on=1, col=%d)", col))
}