polyRegression <- function (Symbol)
{
  require(quantmod)

  this.env <- environment()
  
  x <- as.integer(index(Symbol))
  y <- as.double((Hi(Symbol)+Lo(Symbol)+Cl(Symbol))/3)

  o = order(x)

  x <- as.Date(index(Symbol))

  r <- lm(y~poly(x,2))

  yp <- predict(r)

  yr <- xts(yp, as.Date(x))
  
  diffReg <- diff(yr)
  diffVal <- xts(y-yp, as.Date(x))

  return(list(regression=yr, diffReg=diffReg, diffVal=diffVal, sigma=summary(r)$sigma))
}

findBestCurve <- function(SymbolName, minDays, maxDays)
{
  minSigmaPeriod <- minDays
  minSigmaValue  <- Inf
  
  for(i in minDays:maxDays)
  {
    periodString <- sprintf("%d days", i)
    lista <- polyRegression(last(get(SymbolName), periodString))
    if(lista$sigma < minSigmaValue)
    {
      minSigmaPeriod <- i
      minSigmaValue  <- lista$sigma
    }
  }
  
  result <- sprintf("Minimo %s %s", minSigmaPeriod, minSigmaValue)
  print(result)  
  
  periodString <- sprintf("%d days", minSigmaPeriod)
  lista <- polyRegression(last(get(SymbolName), periodString))
  periodString2 <- sprintf("%d days", minSigmaPeriod*2)
  plotPolyReg(last(get(SymbolName), periodString2), lista$regression, 2*lista$sigma)
}

