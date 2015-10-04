library("RcppEigen")
library("foreach")


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

  r <- RcppEigen::fastLm(poly(x,2), y)
  
  yp <- predict(r)+mean(r$residuals)
  
  yr <- xts(yp, as.Date(x))
  
  return(list(regression=yr, sigma=sd(r$residuals), name=SymbolName,
              interval=DateInterval, trend=revertTrend(yr, n=3), period=Period))
}


findCurves <- function(SymbolName, minDays, maxDays, dateLimit="")
{
  if(dateLimit == "")
  {    
    dt = as.Date(format(Sys.time(), "%Y-%m-%d"))
  }
  else
  { 
    dt = as.Date(dateLimit)
  }
  
  lista <- foreach (i = minDays:maxDays, .combine = rbind, .errorhandling="remove") %do%
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

computeRegressions <- function(Symbol, StartDate, EndDate)
{
  k1 <- 10
  k2 <- 200
  
  lista <- NULL
  
  for(dt in seq(as.Date(EndDate), as.Date(StartDate), by = "-1 day"))
  {
    chartDate <- sprintf("%s", as.Date(dt))
    
    if(length(get(Symbol)[chartDate]) > 0)
    {
      alertas <- findCurves(Symbol, k1, k2, dateLimit=chartDate)
      
      if(length(alertas) > 0)
      {
        assign(sprintf("%s.regset", Symbol), alertas, .GlobalEnv)
        item <- filterObjectsSets(Symbol, chartDate)
        
        if(is.null(item) == FALSE)
        {
          lista <- c(lista, item)
        }
      }
    }
  }
  
  if(is.null(lista))
  {
    lista <- FALSE
  }
  
  return(lista)
}

changeRatio <- function(regIndicator)
{
  first <- as.Date(substr(regIndicator$interval, 1, 10))
  last  <- as.Date(substr(regIndicator$interval, 13, 23))
  
  days <- as.integer(difftime(last, first))
             
  return(abs(as.double(first(regIndicator$regression)) - as.double(last(regIndicator$regression))) / (days/30))
}

getPolyRegs <- function(Symbol, endDate=NULL)
{
  ptrnStr <- sprintf(".*%s.*r_.*rds", Symbol)
  objFiles <- list.files("data", pattern=ptrnStr)
  
  print(Symbol)

  if(is.null(endDate))
    endDate <- Sys.Date()
  
  k <- 1
  polyRegs <- c()
  
  for(name in objFiles)
  {
    fileName <- sprintf("data/%s", name)
    alertas <- readRDS(file=fileName)
    
    if(length(alertas) > 0)
    {
      for(i in 1:(length(alertas)-1))
      {
        if(as.Date(substr(name, 1, 10)) <= endDate && changeRatio(alertas[[i]]) > 1.5) #1.5% a.m.
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
  }
  
  return(polyRegs)
}
