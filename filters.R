source('poly-reg.R')
source('utils.R')


loadFilters <- function(Symbols = NULL, Filters = NULL)
{
  str(Symbols)
  for (i in 1:length(Symbols))
  {
    for (j in 1:length(Filters))
    {
      #str(Symbols[i])
      #str(Filters[j])
      #get(Symbols[i])
      #Symbols[[i]] <- merge(Symbols[[i]], as.numeric(0))# colocar o nome
    }
  }
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

filterIncomplete <- function(SymbolNames=NULL, dateLimit="")
{
  if(is.null(SymbolNames))
  {
    return
  }
  
  symbols <- c()
  j <- 1
  for(i in 1:length(SymbolNames))
  {
    if(dateLimit == "")
    {
      period <- sprintf("%s::%s", as.Date(Sys.Date() - 30), as.Date(Sys.Date()))
    }
    else
    {
      period <- sprintf("%s::%s", as.Date(as.Date(dateLimit) - 30), as.Date(dateLimit))
    }
    lastMonth <- get(SymbolNames[[i]])[period]
    
    lastMonthDays <- length(lastMonth[,1])
    if(lastMonthDays >= 15)
    {
      symbols[j] <- SymbolNames[[i]]
      j <- j + 1
    }
  }
  
  return (symbols)
}

filterMultiple <- function(SymbolNames, Filters)
{
  
  filterSymbols <- SymbolNames
  
  for(i in 1:length(Filters))
  {
    symbols <- c()
    k <- 0
    
    for(j in 1:length(SymbolNames))
    {
      #if(Filters = "1sigma")
      #{
      #  symbols[k] <- filterPolyReg(Wallet, 60, 180, maxSigma=1.0)
      #  k++
      #}
      #else if(Filters = "2sigma")
      #{
      #  
      #}
      #else if(Filters = )
      #{
      #  
      #}     
    }
    
    filterSymbols <- symbols
  }
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

filterObjectsSets <- function(Symbols, startDate, endDate)
{
  k1 <- 10
  k2 <- 730
  
  k <- 1
  symbolList <- c()
  
  for(dt in seq(as.Date(endDate), as.Date(startDate), by = "-1 day"))
  {
    chartDate <- sprintf("%s", as.Date(dt))
    
    filterSymbols <- filterIncomplete(Symbols)
    
    for(symbol in filterSymbols)
    {
      if(length(get(symbol)[chartDate]) == 0)
        next
      
      strOut <- sprintf("filterRevert %s %d %d %s", symbol, k1, k2, chartDate)
      print(strOut)
      
      objectName <- sprintf("backtest/%s-%s_%d_%d.rds", chartDate, symbol, k1, k2)
      
      if(file.exists(fileName=objectName) == FALSE)
      {
        next
      }
      
      alertas <- readRDS(file=objectName)
      
      if(length(alertas) == 0)
      {
        next
      }
      
      alertas <- turnPoints(alertas)
      
      strOut <- sprintf("%s %s: %d", symbol, chartDate, length(turnPoints))
      print(strOut)
      
      if(length(alertas) > 0)
      {
        objectName <- sprintf("backtest_dir/%s-%s_%d_%d_turnpoints.rds", chartDate, symbol, k1, k2)
        
        saveRDS(alertas, file=objectName)
      }
      if(length(alertas) == 0)
      {
        next
      }
      
      trends <- c("r_up")
      alertas_r_up <- filterRevert(alertas, trends, 3)
      
      if(length(alertas_r_up) > 0)
      {
        objectName <- sprintf("backtest_dir/%s-%s_%d_%d_turnpoints_r_up.rds", chartDate, symbol, k1, k2)
        
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
        objectName <- sprintf("backtest_dir/%s-%s_%d_%d_turnpoints_r_down.rds", chartDate, symbol, k1, k2)
        
        if((symbol %in% symbolList) == FALSE)
        {
          symbolList[k] <- symbol
          k <- k+1
        }
        
        saveRDS(alertas_r_dow, file=objectName)
      }
    }
  }
  
  return(symbolList)
}
