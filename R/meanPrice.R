source("R/dbInterface.R")

getMeanPrice <- function(Symbol, SymbolName)
{
  mePrice <- meanPrice(SymbolName)

  if(is.null(mePrice))
    return(NULL)

  mPrice = xts(rep(mePrice,length(index(Symbol))), index(Symbol))

  assign("mPrice", mPrice, .GlobalEnv)

  col <- 3
  if(as.double(last(Cl(Symbol))) < mePrice)
    col <- 2

  return(sprintf("addTA(mPrice, lwd=1, on=1, col=%d)", col))
}
