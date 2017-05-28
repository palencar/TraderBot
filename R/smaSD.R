smaSD <- function(Symbol, n=200)
{
  seq <- as.xts((Hi(Symbol)+Lo(Symbol)+Cl(Symbol))/3)

  if((nrow(seq) - n - 500) < 0)
  {
    warning(sprintf("sma(%d, %d)", length(seq), n))
    return(NULL)
  }

  #compute sd
  sma <- SMA(seq, n) #roughtly 2 years (on daily timeframe)
  ssd <- sd(as.double(na.omit(seq-sma)))

  #compute sma for all the data
  obj<-Symbol
  seq <- as.xts((Hi(obj)+Lo(obj)+Cl(obj))/3)
  sma <- SMA(seq, n)

  smaSd <- c()

  objName <- sprintf("sma%d", n)
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
