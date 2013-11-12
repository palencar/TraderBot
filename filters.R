source("polyReg.R")
library(pastecs)


turnPoints <- function(object, maxTpoints=8)
{
  sigmas <- c()
  for(i in 1:length(object))
  {
    reg <- object[[i]]
    
    if(is.na(reg$sigma))
    {
      sigmas[[i]] <- Inf
    }
    else
    {
      sigmas[[i]] <- reg$sigma 
    }
  }
  
  if(length(sigmas) < maxTpoints)
    return(sigmas)
  
  tp <- extract(turnpoints(sigmas), 100000, peak=0, pit=1)
  tPoints <- c()
  
  Tp <- object[[length(tp)]]$period
  for(i in length(tp):1)
  {
    if(tp[i] == 1)
    {
      tPoints[[i]] <- object[[i]]$sigma
      Tp <- object[[i]]$sigma
    }
    else
    {
      tPoints[[i]] <- Tp
    }
  }
  
  #if(length(sigmas) < maxTpoints)
  #  return(sigmas)
  
  tp <- extract(turnpoints(tPoints), 100000, peak=0, pit=1)
  
  k <- 1
  lista <- c()
  
  for(i in 1:length(tp))
  {
    if(tp[i] == 1)
    {
      lista[[k]] <- object[[i]]
      k <- k+1
    }
  }
  
  return(lista)
}

filterPolyReg <- function(SymbolNames, minDays, maxDays, minSigma=0, maxSigma=0, dateLimit="")
{
  j <- 1
  lista <- list()
  names <- c()
  
  for(i in 1:length(SymbolNames))
  {
    reg <- findBestCurve(SymbolName=SymbolNames[i], minDays=minDays, maxDays=maxDays, dateLimit=dateLimit)
    
    lastDayDate <- time(xts::last(reg$regression))
    
    if(minSigma != 0)
    {
      if(Lo(get(SymbolNames[i])[lastDayDate]) < xts::last(reg$regression[lastDayDate])+(minSigma*reg$sigma))
      {
        lista[[j]] <- reg
        names[[j]] <- reg$name
        j <- j + 1
      }
    }
    
    if(maxSigma != 0)
    {
      if(Lo(get(SymbolNames[i])[lastDayDate]) > xts::last(reg$regression[lastDayDate])+(maxSigma*reg$sigma))
      {
        lista[[j]] <- reg
        names[[j]] <- reg$name
        j <- j + 1
      }
    }
    
    if(minSigma == 0 && maxSigma == 0)
    {
      lista[[j]] <- reg
      names[[j]] <- reg$name
      j <- j + 1
    }
  }
  
  lista$names <- names
  
  return(lista)
}

revertTrend <- function(TimeSeries, n=3)
{
  lastValues <- xts::last(TimeSeries, n)
  
  trend <- "none"
  
  for(i in 2:length(lastValues))
  {
    if(as.numeric(lastValues[i-1]) < as.numeric(lastValues[i]))
    {
      if(trend == "down")
      {
        return("r_up")
      }
      
      trend <- "up"
    }
    
    if(as.numeric(lastValues[i-1]) > as.numeric(lastValues[i]))
    {
      if(trend == "up")
      {
        return("r_down")
      }
      
      trend <- "down"
    }
  }
  
  return(trend)
}

filterRevert <- function(Regressions, trend=NULL, period=NULL)
{
  j <- 1
  lista <- c()
  names <- c()
  
  for(i in 1:length(Regressions))
  {
    reg <- Regressions[[i]]
    
    treg <- reg$regression
    if(length(treg) == 0)
    {
      print("zero")
      print(reg$name)
    }
    
    if(is.null(period))
    {
      dtrend <- revertTrend(treg, n=length(treg))
    }
    else
    {
      dtrend <- revertTrend(treg, n=period) 
    }
    
    if(dtrend %in% trend)
    {
      lista[[j]] <- reg
      names[[j]] <- reg$name
      lista[[j]]$trend <- dtrend
      
      j <- j + 1 
    }
  }
  
  lista$names <- names
  
  return(lista)
}

filterLRI <- function(symbol, lri, threshold=1)
{
  r <- rle(sign(diff(as.vector(lri))))
  
  len <- length(r$values)
  
  if(len <= 3)
  {
    return(FALSE)
  }
  
  rdif <- c()
  
  lastIndex <- 1
  for(i in 1:len)
  {
    nextIndex <- lastIndex + r$lengths[i]
    
    if(r$values[i] == 1)
    {
      high <- as.double(Hi(symbol[lastIndex]))
      low  <- as.double(Lo(symbol[nextIndex]))
      
      dif <-  (high/low)/low
    }
    else if(r$values[i] == -1)
    {
      high <- as.double(Hi(symbol[lastIndex]))
      low  <- as.double(Lo(symbol[nextIndex]))
      
      dif <- (high-low)/high
    }
    else
    {
      dif <= 0.0
    }
    
    rdif[i] <- dif
    lastIndex <- nextIndex
  }
  
  alert <- c()
  sdev <- sd(rdif)
  
  for(i in 1:len)
  {
    if(rdif[i] > 0)
    {
      alert[i] <- if(rdif[i] >= (sdev*threshold)) TRUE else FALSE
    }
    else if(rdif[i] < 0)
    {
      alert[i] <- if(rdif[i] <= (-sdev*threshold)) TRUE else FALSE
    }
    else
    {
      alert[i] <- FALSE
    }
  }
  
  if(r$values[len] == 1 && r$values[len] == -1) # >, <
  {
    if(r$lengths[len] == 1 && alert[len-1])
    {
      return(TRUE)
    }
  }
  
  return(FALSE)
}

filterIncomplete <- function(SymbolNames=NULL, dateLimit="")
{
  if(is.null(SymbolNames))
  {
    return
  }
  
  symbols <- c()
  j <- 1
  for(i in SymbolNames)
  {
    if(length(get(i)) < 60)
    {
      next
    }
    
    if(dateLimit == "")
    {
      period <- sprintf("%s::%s", as.Date(Sys.Date() - 30), as.Date(Sys.Date()))
    }
    else
    {
      period <- sprintf("%s::%s", as.Date(as.Date(dateLimit) - 30), as.Date(dateLimit))
    }
    lastMonth <- get(i)[period]
    
    lastMonthDays <- length(lastMonth[,1])
    if(lastMonthDays >= 15)
    {
      symbols[j] <- i
      j <- j + 1
    }
  }
  
  return (symbols)
}

filterAge <- function(SymbolNames, dateLimit="", age="6 months")
{
  if(dateLimit == "")
  {
    dt = as.Date(Sys.Date())
  }
  else
  {
    dt = as.Date(dateLimit)
  }
  
  dc = sprintf("-%s", age)
  
  ds = seq(dt, length=2, by=dc)[2]
  
  symbols <- c()
  
  i <- 1
  for(symb in SymbolNames)
  {
    period <- sprintf("::%s", ds)

    print(period)
    if(length(get(symb)[period]) > 0)
    {
      print(period)
      print(symb)
      
      symbols[i] <- symb
      i <- i+1
    }
  }
  
  return(symbols)
}

filterVolume <- function(SymbolNames, volume=10000, dateLimit="", age="6 months")
{
  if(dateLimit == "")
  {
    dt = as.Date(Sys.Date())
  }
  else
  {
    dt = as.Date(dateLimit)
  }
  
  dc = sprintf("-%s", age)
  
  ds = seq(dt, length=2, by=dc)
  
  symbols <- c()
  
  i <- 1
  for(symb in SymbolNames)
  {
    period <- sprintf("%s::%s", ds[2], ds[1])
    
    if(length(get(symb)[period]) > 0)
    {
      print(period)
      print(symb)
      
      symbols[i] <- symb
      i <- i+1
    }
  }
  
  return(symbols)
}

filterObjectsSets <- function(Symbols, ChartDate)
{
  k1 <- 10
  k2 <- 730
  
  k <- 1
  symbolList <- c()
  
  filterSymbols <- filterIncomplete(Symbols)
  
  for(symbol in filterSymbols)
  {
    if(length(get(symbol)[ChartDate]) == 0)
    {
      next
    }
    
    if(exists(sprintf("%s.regset", symbol)) == FALSE)
    {
      next
    }
    
    regset <- get(sprintf("%s.regset", symbol))
    
    if(exists(sprintf("%s.regset", symbol), envir=.GlobalEnv) == TRUE)
    {
      rm(list=sprintf("%s.regset", symbol), envir=.GlobalEnv)
    }
    
    if(length(regset) == 0)
    {
      next
    }
    
    print(sprintf("filterRevert %s %d %d %s", symbol, k1, k2, ChartDate))
    
    alertas <- turnPoints(regset)
    
    #strOut <- sprintf("%s %s: %d", symbol, ChartDate, length(turnPoints))
    #print(strOut)
    #
    #if(length(alertas) > 0)
    #{
    #  objectName <- sprintf("backtest_dir/%s-%s_%d_%d_turnpoints.rds", ChartDate, symbol, k1, k2)
    #  
    #  saveRDS(alertas, file=objectName)
    #}
    #if(length(alertas) == 0)
    #{
    #  next
    #}
    
    trends <- c("r_up")
    alertas_r_up <- filterRevert(alertas, trends, 3)
    
    if(length(alertas_r_up) > 0)
    {
      objectName <- sprintf("backtest_dir/%s-%s_%d_%d_turnpoints_r_up.rds", ChartDate, symbol, k1, k2)
      
      if((symbol %in% symbolList) == FALSE)
      {
        symbolList[k] <- symbol
        k <- k+1
      }
      
      saveRDS(alertas_r_up, file=objectName)
    }
    
    trends <- c("r_down")
    alertas_r_dow <- filterRevert(alertas, trends, 3)
    
    if(length(alertas_r_dow) > 0)
    {
      objectName <- sprintf("backtest_dir/%s-%s_%d_%d_turnpoints_r_down.rds", ChartDate, symbol, k1, k2)
      
      if((symbol %in% symbolList) == FALSE)
      {
        symbolList[k] <- symbol
        k <- k+1
      }
      
      saveRDS(alertas_r_dow, file=objectName)
    }
  }
  
  return(symbolList)
}
