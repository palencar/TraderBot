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
  
  chartSeries(Symbol)
  
  sigma <- summary(r)$sigma  
  plot(addTA(yr, on=1, col=3))
  plot(addTA(yr+sigma, on=1, col=7))
  plot(addTA(yr-sigma, on=1, col=7))
  
  diffReg <- diff(yr)
  diffVal <- xts(y-yp, as.Date(x))

  return(list(diffReg=diffReg, diffVal=diffVal, sigma=sigma))
}