source("polyReg.R")
library(pastecs)


turnPoints <- function(object, maxTpoints=8)
{
  sigmas <- c()
  for(i in 1:length(object))
  {
    reg <- object[[i]]
    
    if(is.null(reg))
    {
      sigmas[[i]] <- Inf
    }
    else if(is.na(reg$sigma))
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

filterLRI <- function(symbol, lri, threshold=1.2)
{
  r <- rle(sign(diff(as.vector(lri))))
  
  len <- length(r$values)
  
  if(r$lengths[len] > 1)
  {
    return(FALSE)
  }
  
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
      high <- as.double(lri[nextIndex])
      low  <- as.double(lri[lastIndex])
      
      dif <- (high-low)/low
    }
    else if(r$values[i] == -1)
    {
      high <- as.double(lri[lastIndex])
      low  <- as.double(lri[nextIndex])
      
      dif <- (low-high)/high
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
 
  if(r$values[len-1] == -1 && r$values[len] == 1)
  {
    if(rdif[len-1] <= (-sdev*threshold))
    {
      return(TRUE)
    }
  }
  
  if(r$values[len-1] == 1 && r$values[len] == -1)
  {
    if(rdif[len-1] >= (sdev*threshold))
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
  
  tradeDays <- getQuery(user="paulo", dbname="beancounter", queryStr="select distinct date from stockprices where date >= (select now() - interval 2 year) order by date desc")[,1]
  firstTradeDay <- last(tradeDays)
  
  symbols <- NULL
  k <- 0

  for(symbol in SymbolNames)
  {
    obj <- get(symbol)
    if(length(obj) < 60)
    {
      next
    }
    
    if((sum(is.na(as.numeric(Op(obj)))) + sum(is.na(as.numeric(Hi(obj)))) + sum(is.na(as.numeric(Lo(obj)))) + sum(is.na(as.numeric(Cl(obj))))) > 0)
    {
      next
    }
    
    print(symbol)
    
    err <- 0
    lastError <- NULL
    exclude <- FALSE
    
    for(i in 1:length(tradeDays))
    {
      cdate <- tradeDays[i]
      if(length(obj[cdate]) == 0)
      {
        lastError <- cdate

        k <- 0
        for(j in (i+1):length(tradeDays))
        {
          if(length(obj[tradeDays[j]]) == 0)
          {
            print(sprintf("Bad data on symbol %s[%s]", symbol, lastError))
            
            if(as.integer(as.Date(lastError) - as.Date(tradeDays[j])) < 2)
              k <- k + 1
            
            lastError <- tradeDays[j]
            
            if(k >= 3)
            {
              exclude <- TRUE
              break
            }
          }
        }
      }
      if(exclude == TRUE)
      {
        break
      }
    }
    
    if(exclude == TRUE)
    {
      print(sprintf("excluding %s from symbols %s", symbol, lastError))
      next
    }
    
    symbols <- c(symbols, symbol)
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
  
  for(symbol in Symbols)
  {
    if(length(get(symbol)[ChartDate]) == 0)
    {
      next
    }
    
    if(exists(sprintf("%s.regset", symbol)) == FALSE)
    {
      next
    }
    
    regset <- get(sprintf("%s.regset", symbol), envir=.GlobalEnv)
    
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
