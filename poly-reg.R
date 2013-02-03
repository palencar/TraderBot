polyRegression <- function (Symbol)
{
  require(quantmod)

  this.env <- environment()
  
  x <- as.integer(index(Symbol))
  if(is.HLC(Symbol))
  {
    y <- as.double((Hi(Symbol)+Lo(Symbol)+Cl(Symbol))/3)
  }
  else
  {
    y <- as.double(Symbol[,1])
  }

  o = order(x)

  x <- as.Date(index(Symbol))

  r <- lm(y~poly(x,2))

  yp <- predict(r)
  
  lastDay <- as.Date(last(index(Symbol)))
  next10Day <- as.Date(lastDay + 10)
  dataextra<-data.frame(x=seq(lastDay,next10Day,1))
  
  ep <- predict(lm(y~poly(x,2)),dataextra) 

  yr <- xts(yp, as.Date(x))
  
  diffReg <- diff(yr)
  diffVal <- xts(y-yp, as.Date(x))

  return(list(regression=yr, diffReg=diffReg, diffVal=diffVal, sigma=summary(r)$sigma))
}

findBestCurve <- function(SymbolName, minDays, maxDays, dateLimit="")
{
  minSigmaPeriod <- minDays
  minSigmaValue  <- Inf
  
  for(i in minDays:maxDays)
  {
    if(dateLimit == "")
    {
      dt = as.Date(format(Sys.time(), "%Y-%m-%d"))
      dc = sprintf("-%d days", i)
      ds = seq(dt, length=2, by=dc)
      
      dateInterval <- sprintf("%s::%s", ds[2], ds[1])
    }
    else
    {
      dt = as.Date(dateLimit)
      dc = sprintf("-%d days", i)
      ds = seq(dt, length=2, by=dc)
      
      dateInterval <- sprintf("%s::%s", ds[2], ds[1])
    }
    
    lista <- polyRegression(get(SymbolName)[dateInterval])
    if(lista$sigma < minSigmaValue)
    {
      minSigmaPeriod <- i
      minSigmaValue  <- lista$sigma
      minSigmaInterval <- dateInterval
      minPolyRegression <- lista
    }
  }
  
  result <- sprintf("Minimo %d [%s] %.4f", minSigmaPeriod, minSigmaInterval, minSigmaValue)
  #print(result)
  
  lista <- minPolyRegression
  
  lista$name <- SymbolName
  lista$period <- minSigmaPeriod
  lista$interval <- minSigmaInterval

  return(lista)
}

findRevertCurves <- function(FilterSymbols, minDays, maxDays, trend=c("r_up", "r_down"), period=3, dateLimit="")
{
  lista <- c()
  
  minSigmaPeriod <- minDays
  minSigmaValue  <- Inf
  
  k <- 0
  
  for(j in 1:length(FilterSymbols))
  {
    SymbolName <- FilterSymbols[j]

    for(i in minDays:maxDays)
    {
      if(dateLimit == "")
      {
        dt = as.Date(format(Sys.time(), "%Y-%m-%d"))
        dc = sprintf("-%d days", i)
        ds = seq(dt, length=2, by=dc)
        
        dateInterval <- sprintf("%s::%s", ds[2], ds[1])
      }
      else
      {
        dt = as.Date(dateLimit)
        dc = sprintf("-%d days", i)
        ds = seq(dt, length=2, by=dc)
        
        dateInterval <- sprintf("%s::%s", ds[2], ds[1])
      }
      
      reg <- polyRegression(get(SymbolName)[dateInterval])
      dtrend <- revertTrend(reg$regression, n=period)

      if(dtrend %in% trend)
      {
        k <- k + 1
        
        lista[[k]]          <- reg
        lista[[k]]$name     <- SymbolName
        lista[[k]]$interval <- dateInterval
        lista[[k]]$trend    <- dtrend
        lista[[k]]$period   <- i
        
        result <- sprintf("%s %s %d", lista[[k]]$name, lista[[k]]$trend, lista[[k]]$period)
        print(result)
      }
    }
  }
  
  return(lista)
}
