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
  
  lastDate  <- as.Date(xts::last(index(Symbol)))
  firstDate <- as.Date(lastDate-window)
  
  dateInterval <- Symbol[sprintf("%s/%s", firstDate, lastDate)]
  
  for(i in 1:nrow(dateInterval))
  {
    xDate <- as.Date(index(dateInterval[i]))
    
    startDate <- as.Date(xDate-n)
    endDate   <- as.Date(xDate)
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
    lri[i] <- predict(lm(y~poly(x,2)), dataextra)[2]
  }
  
  xi <- xts(lri, index(dateInterval))
  
  return(xi)
}

getLinRegIndicators <- function(SymbolName, n=c(50))
{
  polyRegs <- c()
  
  for(i in n)
  {
    objName <- sprintf("lri%s.p%d", SymbolName, i)
    Symbol <- get(SymbolName)
    obj <- linearRegressionIndicator(Symbol, window=(length(index(Symbol))-i), n=i)
    assign(objName, obj, .GlobalEnv)
    polyRegs <- c(polyRegs, sprintf("addTA(%s, on=1, col=3)", objName))
  }
  
  return(polyRegs)
}
