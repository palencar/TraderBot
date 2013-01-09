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

  yr <- xts(yp, as.Date(x))
  
  diffReg <- diff(yr)
  diffVal <- xts(y-yp, as.Date(x))

  return(list(regression=yr, diffReg=diffReg, diffVal=diffVal, sigma=summary(r)$sigma))
}

findBestCurve <- function(SymbolName, minDays, maxDays, dateLimit="")
{
  minSigmaPeriod <- minDays
  minSigmaValue  <- Inf
  
  if(dateLimit == "")
  {
    dateLimit <- sprintf("::%s", format(Sys.time(), "%Y-%m-%d"))
  }
  
  for(i in minDays:maxDays)
  {
    periodString <- sprintf("%d days", i)

    lista <- polyRegression(last(get(SymbolName)[dateLimit], periodString))
    if(lista$sigma < minSigmaValue)
    {
      minSigmaPeriod <- i
      minSigmaValue  <- lista$sigma
    }
  }
  
  result <- sprintf("Minimo %s %s", minSigmaPeriod, minSigmaValue)
  
  periodString <- sprintf("%d days", minSigmaPeriod)
  lista <- polyRegression(last(get(SymbolName)[dateLimit], periodString))
  lista$name <- SymbolName
  lista$period <- minSigmaPeriod

  return(lista)
}
