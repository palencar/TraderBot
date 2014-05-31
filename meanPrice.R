source("startProbe.R")

getMeanPrice <- function(Symbol, SymbolName)
{
  mePrice <- meanPrice(SymbolName)
  
  if(is.na(mePrice))
    return(null)
    
  as.Date(index(Symbol))
  mPrice = xts(rep(mePrice,length(index(Symbol))), index(Symbol))
  
  col <- 3
  if(as.double(Cl(last(Symbol))) < mePrice)
    col <- 2
  
  return(sprintf("addTA(mPrice, lwd=1, on=1, col=%d)", col))
}