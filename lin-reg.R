linearRegression <- function (Symbol)
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

  r <- lm(y~x)

  yp <- predict(r)

  yr <- xts(yp, as.Date(x))
  
  sigma <- summary(r)$sigma
  
  diffReg <- diff(yr)
  diffVal <- xts(y-yp, as.Date(x))

  return(list(regression=yr, diffReg=diffReg, diffVal=diffVal, sigma=sigma))
}

linearRegressionIndicator <- function (Symbol, window=365, n=50)
{
  require(quantmod)
  
  this.env <- environment()
  
  lri <- c()
  
  xi <- xts( , index(Symbol))
  
  for(i in 0:(window-1))
  {
    lastDate <- as.Date(xts::last(index(Symbol)))
    startDate <- as.Date(lastDate-(window+n)+i)
    endDate   <- as.Date(lastDate-(window)+i)
    subsetSymbol <- Symbol[sprintf("%s::%s", startDate, endDate)]
    x <- as.integer(index(subsetSymbol))
    if(is.HLC(subsetSymbol))
    {
      y <- as.double((Hi(subsetSymbol)+Lo(subsetSymbol)+Cl(subsetSymbol))/3)
    }
    else
    {
      y <- as.double(subsetSymbol[,1])
    }
    
    o = order(x)
    
    x <- as.Date(index(subsetSymbol))
    
    r <- lm(y~x)
    
    lastDay <- as.Date(last(index(subsetSymbol)))
    dataextra <-data.frame(x=seq(lastDay, as.Date(lastDay + 1), 1))
    lri[i+1] <- predict(lm(y~poly(x,2)), dataextra)[2]
  }
  
  str(lri)
  #xi <- xts(lri, index(Symbol))
  
  return(xi)
}