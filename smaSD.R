smaSD <- function(Symbol, n=200)
{
  obj<-Symbol[paste(rev(seq(as.Date(index(tail(Symbol,1))), length=2, by="-4 years")),collapse = "::")]
  
  seq <- as.xts((Hi(obj)+Lo(obj)+Cl(obj))/3)
  
  if(length(seq) < n)
  {
    warning(sprintf("sma(%d, %d)", length(seq), n))
    return(NULL)
  }
  
  sma <- SMA(seq, n)
  
  ssd <- sd(as.double(na.omit(seq-sma)))
  
  smaSd <- c()
  
  objName <- "sma200"
  assign(objName, sma, .GlobalEnv)
  smaSd <- c(smaSd, sprintf("addTA(%s, on=1, col=2)", objName))
  
  objName <- "smaP2sd"
  assign(objName, sma+(2*ssd), .GlobalEnv)
  smaSd <- c(smaSd, sprintf("addTA(%s, on=1, col=8)", objName))
  
  objName <- "smaM2sd"
  assign(objName, sma-(2*ssd), .GlobalEnv)
  smaSd <- c(smaSd, sprintf("addTA(%s, lwd=2, on=1, col=8)", objName, col))
  
  return(smaSd)
}
