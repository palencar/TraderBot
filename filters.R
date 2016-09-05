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
  lista <- NULL
  names <- NULL
  k <- 1
  
  for(reg in Regressions)
  {
    treg <- reg$regression
    if(length(treg) == 0)
    {
      print("zero")
      print(reg$name)
    }
    
    if(length(reg$interval) == 0)
    {
      print("nulo")
      print(reg$name)
    }
    
    if(is.null(reg) || changeRatio(reg) < 1.5) #1.5% a.m.
    {
      next
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
      reg$trend <- dtrend
      lista[[k]] <- reg
      k <- k+1
      names <- c(names, reg$name)
    }
  }
  
  if(is.null(lista) == FALSE)
  {
    lista$names <- names
  }
  
  return(lista)
}

filterLRI <- function(lri, threshold=0.6)
{
  r <- rle(sign(diff(as.vector(lri))))
  
  len <- length(r$values)
  
  if(r$lengths[len] > 1)
  {
    return("none")
  }
  
  if(len <= 3)
  {
    return("none")
  }
  
  rdif <- c()
  
  lastIndex <- 1
  for(i in 1:len)
  {
    nextIndex <- lastIndex + r$lengths[i]
    rdif[i] <- 0
    if(r$values[i] == 1)
    {
      high <- as.double(lri[nextIndex])
      low  <- as.double(lri[lastIndex])
      rdif[i] <- (high-low)/low
    }
    else if(r$values[i] == -1)
    {
      high <- as.double(lri[lastIndex])
      low  <- as.double(lri[nextIndex]) 
      rdif[i] <- (low-high)/high
    }
    lastIndex <- nextIndex
  }
  
  alert <- c()
  sdev <- sd(rdif)
  lastSignal <- "none"
  
  for(i in 2:len)
  {
    if(r$values[i] == 1 && (rdif[i-1] <= (-sdev*threshold)))#tooo talvez funcione tudo dentro do loop
    {
      lastSignal <- "up"
    }
    
    if(r$values[i] == -1 && (rdif[i-1] >= (sdev*threshold)))
    {
      lastSignal <- "down"
    }
  }
  
  if(r$values[len] == 1 && (rdif[len-1] <= (-sdev*threshold) || lastSignal == "up"))
  {
    return("up")
  }
  
  if(r$values[len] == -1 && (rdif[len-1] >= (sdev*threshold) || lastSignal == "down"))
  {
    return("down")
  }
  
  return("none")
}

filterIncomplete <- function(SymbolNames=NULL, dateLimit="NOW")
{
  if(is.null(SymbolNames))
  {
    return(NULL)
  }
  
  cacheFileName <- "data/filter.rds"
  filterMap <- new.env(hash=T, parent=emptyenv())
  if(file.exists(cacheFileName))
  {
    filterMap <- readRDS(cacheFileName)
  }
  
  queryStr <- sprintf("select distinct date from stockprices where date >= (select date('%s','-1 year')) order by date desc", dateLimit)
  allTradeDays <- getQuery(queryStr)[,1]
  
  if(dateLimit == "NOW")
  {
    dateLimit <- Sys.Date()
  }
  
  symbols <- NULL
  k <- 0

  badData <- NULL
  
  for(symbol in SymbolNames)
  {  
    obj <- get(symbol)
    if(length(obj) < 60)
    {
      next
    }
    
    if(anyNA(as.numeric(OHLCV(obj))))
    {
      next
    }

    err <- 0
    exclude <- FALSE
    tradeDays <- allTradeDays[which(allTradeDays <= as.Date(dateLimit))]
    lastError <- tradeDays[1]
    
    goodDate <- NULL
    if(!is.null(filterMap))
    {
      goodDate <- filterMap[[symbol]]
    }
    
    if(!is.null(goodDate))
    {
      tradeDays <- tradeDays[which(tradeDays >= as.Date(goodDate))]
    }
    
    k <- 0
    for(tradeDay in tradeDays)
    {
      if(length(obj[tradeDay]) == 0)
      {
        badData <- c(badData, sprintf("%s %s", symbol, tradeDay))
        
        if(as.integer(as.Date(lastError) - as.Date(tradeDay)) < 2)
          k <- k + 1
        else
          k <- 0
        
        lastError <- tradeDay
        
        if((as.integer(Sys.Date() - as.Date(tradeDay)) < 10) && k >= 1)
        {
          exclude <- TRUE 
        }
        else if((as.integer(Sys.Date() - as.Date(tradeDay)) < 60) && k >= 2)
        {
          exclude <- TRUE
        }
        else if((as.integer(Sys.Date() - as.Date(tradeDay)) < 120) && k >= 3)
        {
          exclude <- TRUE
        }
        else if(k >= 4)
        {
          exclude <- TRUE
        }
      }
      else if(!is.null(goodDate) && as.Date(tradeDay) <= as.Date(goodDate))
      {
        #print(sprintf("good data up to %s", as.Date(tradeDay)))
        break
      }
    }
    
    if(exclude == TRUE)
    {
      warning(sprintf("excluding %s from symbols %s", symbol, lastError))
      next
    }
    
    symbols <- c(symbols, symbol)
    filterMap[[symbol]] <- as.Date(last(index(obj)))
  }
  
  if(!is.null(badData))
    writeLines(badData, "baddata.txt")
  
  saveRDS(filterMap, file=cacheFileName)
  
  exclude <- setdiff(SymbolNames, symbols)
  if(length(exclude) > 0)
  {
    print(sprintf("Incomplete Excuding [%s]: %s", as.Date(dateLimit), paste(exclude, collapse = " ")))
  }
  
  return (symbols)
}

filterGap <- function(SymbolNames=NULL, dateLimit="NOW")
{
  if(is.null(SymbolNames))
  {
    return(NULL)
  }
  
  cacheFileName <- "data/filter.rds"
  filterMap <- new.env(hash=T, parent=emptyenv())
  if(file.exists(cacheFileName))
  {
    filterMap <- readRDS(cacheFileName)
  }
  
  queryStr <- sprintf("select distinct date from stockprices where date >= (select date('%s','-1 year')) order by date desc", dateLimit)
  allTradeDays <- getQuery(queryStr)[,1]
  
  if(dateLimit == "NOW")
  {
    dateLimit <- Sys.Date()
  }
  
  symbols <- NULL
  badData <- NULL
  
  for(symbol in SymbolNames)
  {  
    obj <- get(symbol)
    if(length(obj) < 60)
    {
      next
    }
    
    if(anyNA(as.numeric(OHLCV(obj))))
    {
      next
    }
    
    err <- 0
    exclude <- FALSE
    tradeDays <- allTradeDays[which(allTradeDays <= as.Date(dateLimit))]
    lastError <- tradeDays[1]
    
    goodDate <- NULL
    if(!is.null(filterMap))
    {
      goodDate <- filterMap[[symbol]]
    }
    
    if(!is.null(goodDate))
    {
      tradeDays <- tradeDays[which(tradeDays >= as.Date(goodDate))]
    }
    
    obj <- obj[sprintf("%s/%s", tradeDays[length(tradeDays)], tradeDays[1])]
    
    if(!is.null(goodDate) && !is.null(dateLimit) && as.Date(dateLimit) <= as.Date(goodDate))
    {
      #print(sprintf("good data up to %s", as.Date(dateLimit)))
    }
    else if(is.null(goodDate) && length(index(obj)) <= 3)
    {
      badData <- c(badData, sprintf("%s %s", symbol, dateLimit))
      exclude <- TRUE
    }
    else if(!is.null(goodDate) && length(index(obj)) <= 2 &&
            (as.integer(index(obj[length(index(obj))])) - as.integer(index(obj[1]))) > 10)
    {
      badData <- c(badData, sprintf("%s %s", symbol, dateLimit))
      exclude <- TRUE
    }
    else
    {
      first <- 1
      last <- length(index(obj)) 
      
      d <- 3
      if(last - first < 3)
      {
        d <- last - first
      }
      
      for(i in last:(d+1))
      {
        if(length(obj[i-d]) == 0)
        {
          exclude <- TRUE
          break
        }
        
        if(as.integer(index(obj[i]) - index(obj[i-d])) > 10)
        {
          badData <- c(badData, sprintf("%s %s", symbol, dateLimit))
          exclude <- TRUE
          break
        }
      }
    }
    
    if(exclude == TRUE)
    {
      warning(sprintf("excluding %s from symbols %s", symbol, lastError))
      next
    }
    
    symbols <- c(symbols, symbol)
    filterMap[[symbol]] <- as.Date(last(index(obj)))
  }
  
  if(!is.null(badData))
    writeLines(badData, "baddata.txt")
  
  saveRDS(filterMap, file=cacheFileName)
  
  exclude <- setdiff(SymbolNames, symbols)
  if(length(exclude) > 0)
  {
    print(sprintf("Gap Excuding [%s]: %s", as.Date(dateLimit), paste(exclude, collapse = " ")))
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

filterData <- function(SymbolNames, endDate)
{
  toFilter <- filterVolume(SymbolNames, endDate)
  toFilter <- filterGap(toFilter, endDate)
  toFilter <- filterBadData(toFilter, endDate)
  
  return(toFilter)
}

filterBadData <- function(SymbolNames, dateLimit=NULL)
{
  symbols <- NULL
  
  if(is.null(SymbolNames))
  {
    return(NULL)
  }
  
  if(is.null(dateLimit))
  {
    dateLimit <- lastTradingSession()
  }
  
  cacheFileName <- "data/good.rds"
  filterMap <- new.env(hash=T, parent=emptyenv())
  if(file.exists(cacheFileName))
  {
    filterMap <- readRDS(cacheFileName)
  }
  
  for(symbol in SymbolNames)
  {
    pass <- TRUE

    goodDate <- NULL
    if(!is.null(filterMap))
    {
      goodDate <- filterMap[[symbol]]
      if(!is.null(goodDate))
        goodDate <- as.Date(goodDate)
    }
    
    if(is.null(goodDate) || as.Date(goodDate) < as.Date(dateLimit))
    {
      if(is.null(goodDate))
        goodDate <- ""
      
      obj <- get(symbol)[sprintf("%s/%s", goodDate, dateLimit)]
      mn  <- mean(Cl(obj))
      
      if(length(index(obj)) >= 2)
      {
        for(i in 2:(length(index(obj))))
        {
          prevVal <- as.numeric(OHLC(obj[i-1]))
          curVal  <- as.numeric(OHLC(obj[i]))
          
          if(anyNA(curVal) || any(curVal == 0))
          {
            warning(print(paste(symbol, as.character(as.Date(index(obj[i]))), paste(curVal, collapse = " "))))
            pass <- FALSE
            next
          }
          
          if(anyNA(prevVal) || any(prevVal == 0))
          {
            warning(print(paste(symbol, as.character(as.Date(index(obj[i-1]))), paste(prevVal, collapse = " "))))
            pass <- FALSE
            next
          }
          
          if(max(prevVal) / max(curVal) > 1.5 || max(prevVal) / max(curVal) < 0.5 ||
             min(prevVal) / min(curVal) > 1.5 || min(prevVal) / min(curVal) < 0.5)
          {
            #if(as.Date(index(obj[i])) < as.Date("2008-01-01") && (max(curVal) < 1.0|| max(prevVal) < 1.0))
            #{
            #  warning(print(sprintf("%s %s: Too old or insignificant, ignore..", symbol, as.Date.character(index(obj[i])))))
            #}
            #else
            #{
            #  pass <- FALSE
            #}
            
            warning(print(paste(symbol, as.character(as.Date(index(obj[i-1]))), paste(prevVal, collapse = " "))))
            warning(print(paste(symbol, as.character(as.Date(index(obj[i]))), paste(curVal, collapse = " "))))
            pass <- FALSE
          }
        }
        
        filterMap[[symbol]] <-as.Date(last(index(obj)))
      }
    }
    
    if(pass == TRUE)
    {
      symbols <- c(symbols, symbol)
    }
  }

  saveRDS(filterMap, file=cacheFileName)
  
  exclude <- setdiff(SymbolNames, symbols)
  if(length(exclude) > 0)
  {
    print(sprintf("Bad Data Excuding [%s]: %s", as.Date(dateLimit), paste(exclude, collapse = " ")))
  }
  
  return(symbols)
}

filterVolume <- function(SymbolNames, dateLimit="", age="6 months")
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
  
  for(symb in SymbolNames)
  {
    period <- sprintf("%s::%s", ds[2], ds[1])
    
    obj <- (get(symb)[period])
    
    vol <- as.double(Vo(obj))
    
    if(length(vol) < 90)
    {
      next
    }

    meanVol <- as.double(mean(vol))
    maxVol <- as.double(max(vol))
    
    if(is.null(meanVol) || !is.numeric(meanVol) || is.null(maxVol) || !is.numeric(maxVol))
      next
    
    symbols <- c(symbols, symb)
  }
  
  exclude <- setdiff(SymbolNames, symbols)
  if(length(exclude) > 0)
  {
    print(sprintf("Volume Excuding [%s]: %s", as.Date(dt), paste(exclude, collapse = " ")))
  }
  
  return(symbols)
}

filterObjectsSets <- function(symbol, ChartDate)
{
  k1 <- 10
  k2 <- 730
  
  alerts <- NULL
  
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
  
  alertas <- turnPoints(regset)
  
  trend <- c("r_up")
  alertas_r_up <- filterRevert(alertas, trend, 3)
  
  if(length(alertas_r_up) > 0)
  {
    objectName <- sprintf("data/%s-%s_%d_%d_turnpoints_r_up.rds", ChartDate, symbol, k1, k2)
    
    if((trend %in% alerts) == FALSE)
    {
      alerts <- c(alerts, trend)
    }
    
    saveRDS(alertas_r_up, file=objectName)
  }
  
  trend <- c("r_down")
  alertas_r_dow <- filterRevert(alertas, trend, 3)
  
  if(length(alertas_r_dow) > 0)
  {
    objectName <- sprintf("data/%s-%s_%d_%d_turnpoints_r_down.rds", ChartDate, symbol, k1, k2)
    
    if((trend %in% alerts) == FALSE)
    {
      alerts <- c(alerts, trend)
    }
    
    saveRDS(alertas_r_dow, file=objectName)
  }
  
  return(alerts)
}

filterSMA <- function(rleSeq)
{
  daysUp <- 0
  daysDown <- 0
  
  values <- rleSeq$values[!is.na(rleSeq$values)]
  
  for(i in length(values):1)
  {
    if(is.na(values[i]))
      break
    
    if(values[i] == -1)
      daysDown <- daysDown + rleSeq$lengths[i]
    
    if(values[i] == 1)
      daysUp <- daysDown + rleSeq$lengths[i]
  }
  
  return (as.double((daysUp) / (daysDown + daysUp)))
}
