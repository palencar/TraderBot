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