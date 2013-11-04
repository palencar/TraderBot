library("RcppEigen")
library("multicore")
library("foreach")
library("doMC")


polyRegression <- function (SymbolName, DateInterval, Period)
{
  Symbol <- get(SymbolName)[DateInterval]
  
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

  r <- RcppEigen::fastLmPure(poly(x,2), y)
  
  yp <- predict.mlm(r)+mean(r$residuals)
  
  #lastDay <- as.Date(xts::last(index(Symbol)))
  #next10Day <- as.Date(lastDay + 10)
  #dataextra<-data.frame(x=seq(lastDay,next10Day,1))
  
  #ep <- predict(lm(y~poly(x,2)),dataextra) 
  
  yr <- xts(yp, as.Date(x))
  
  return(list(regression=yr, sigma=sd(r$residuals), name=SymbolName,
              interval=DateInterval, trend=revertTrend(yr, n=3), period=Period))
}


findCurves <- function(SymbolName, minDays, maxDays, dateLimit="")
{
  require(doMC)
  registerDoMC()
  
  if(dateLimit == "")
  {    
    dt = as.Date(format(Sys.time(), "%Y-%m-%d"))
  }
  else
  { 
    dt = as.Date(dateLimit)
  }
  
  lista <- foreach (i = minDays:maxDays, .combine = rbind ) %dopar%
  { 
    list(polyRegression(SymbolName, sprintf("%s::%s", seq(dt, length=2, by=sprintf("-%d days", i))[2], dt), i))
  }
  
  return(lista)
}

findBestCurve <- function(SymbolName, minDays, maxDays, dateLimit="")
{
  minSigmaPeriod <- minDays
  minSigmaValue  <- Inf
  
  if(dateLimit == "")
  {
    dt = as.Date(format(Sys.time(), "%Y-%m-%d"))
  }
  else
  {
    dt = as.Date(dateLimit)
  }
  
  for(i in minDays:maxDays)
  {  
    lista <- polyRegression(SymbolName, sprintf("%s::%s", seq(dt, length=2, by=sprintf("-%d days", i))[2], dt), i)    
    if(lista$sigma < minSigmaValue)
    {
      minSigmaPeriod <- i
      minSigmaValue  <- lista$sigma
      minSigmaInterval <- dateInterval
      minPolyRegression <- lista
    }
  }
  
  lista <- minPolyRegression
  
  lista$name <- SymbolName
  lista$period <- minSigmaPeriod
  lista$interval <- minSigmaInterval
  
  return(lista)
}

computeRegressions <- function(Symbols, StartDate, EndDate)
{
  k1 <- 10
  k2 <- 730
  
  k <- 1
  lista <- c()
  
  for(dt in seq(as.Date(EndDate), as.Date(StartDate), by = "-1 day"))
  {
    chartDate <- sprintf("%s", as.Date(dt))
    
    filterSymbols <- filterIncomplete(Symbols)

    for(symbol in filterSymbols)
    {
      if(length(get(symbol)[chartDate]) > 0)
      {
        strOut <- sprintf("findCurves %s %d %d %s", symbol, k1, k2, chartDate)
        print(strOut)
        
        alertas <- findCurves(symbol, k1, k2, dateLimit=chartDate)
        
        if(length(alertas) > 0)
        {
          assign(sprintf("%s.regset", symbol), alertas, .GlobalEnv)
          item <- filterObjectsSets(symbol, chartDate)
          
          if(is.null(item) == FALSE)
          {
            lista[k] <- item
            k <- k + 1
          }
        }
      }
    }
  }
  
  return(lista)
}

getPolyRegs <- function(Symbol)
{
  ptrnStr <- sprintf(".*%s.*r_.*rds", Symbol)
  objFiles <- list.files("backtest_dir", pattern=ptrnStr)
  
  print(Symbol)
  
  k <- 1
  polyRegs <- c()
  
  for(name in objFiles)
  {
    fileName <- sprintf("backtest_dir/%s", name)
    alertas <- readRDS(file=fileName)
    
    if(length(alertas) > 0)
    {
      for(i in 1:(length(alertas)-1))
      {    
        objName <- sprintf("poly%s.p%d", Symbol, k)
        assign(objName, alertas[[i]]$regression, .GlobalEnv)
        polyRegs <- c(polyRegs, sprintf("addTA(%s, on=1, col=3)", objName))
        
        objName <- sprintf("poly%s.p%dpsigma", Symbol, k)
        assign(objName, alertas[[i]]$regression+alertas[[i]]$sigma, .GlobalEnv)
        polyRegs <- c(polyRegs, sprintf("addTA(%s, lwd=2, on=1, col=7)", objName, col))
        
        objName <- sprintf("poly%s.p%dmsigma", Symbol, k)
        assign(objName, alertas[[i]]$regression-alertas[[i]]$sigma, .GlobalEnv)
        polyRegs <- c(polyRegs, sprintf("addTA(%s, lwd=2, on=1, col=7)", objName, col))
        
        k <- k+1
      }
    }
  }
  
  return(polyRegs)
}
