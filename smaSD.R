smaSD <- function(SymbolName, n=200)
{
  obj <- get(SymbolName)
  
  seq <- as.xts((Hi(obj)+Lo(obj)+Cl(obj))/3)
  sma <- SMA(seq, n)
  
  ssd <- sd(as.double(na.omit(seq-sma)))
  
  smaSd <- c()
  
  objName <- sprintf("sma200%s", SymbolName)
  assign(objName, sma, .GlobalEnv)
  smaSd <- c(smaSd, sprintf("addTA(%s, on=1, col=2)", objName))
  
  objName <- sprintf("smaP2sd%s", SymbolName)
  assign(objName, sma+(2*ssd), .GlobalEnv)
  smaSd <- c(smaSd, sprintf("addTA(%s, on=1, col=8)", objName))
  
  objName <- sprintf("smaM2sd%s", SymbolName)
  assign(objName, sma-(2*ssd), .GlobalEnv)
  smaSd <- c(smaSd, sprintf("addTA(%s, lwd=2, on=1, col=8)", objName, col))
  
  #chartSeries(BBAS3.SA, TA=paste(smaSd, collapse="; "))
  
  return(smaSd)
}
