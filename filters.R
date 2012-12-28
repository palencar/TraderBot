source('poly-reg.R')

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

filterPolyReg <- function(SymbolNames, minDays, maxDays, minSigma=0, maxSigma=0)
{
  j <- 1
  lista <- list()
  names <- c()
  
  for(i in 1:length(SymbolNames))
  {
    reg <- findBestCurve(SymbolNames[i], minDays, maxDays)
    
    lastDayDate <- time(last(reg$regression))
    
    if(minSigma != 0)
    {
      if(Lo(get(SymbolNames[i])[lastDayDate] ) < last(reg$regression[lastDayDate])+(minSigma*reg$sigma))
      {
        lista[[j]] <- reg
        names[[j]] <- reg$name
        j <- j + 1
      }
    }
    
    if(maxSigma != 0)
    {
      if(Lo(get(SymbolNames[i])[lastDayDate] ) > last(reg$regression[lastDayDate])+(maxSigma*reg$sigma))
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

revertTrend <- function(TimeSeries, n=10)
{
  lastValues <- last(TimeSeries, n)
  
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

filterRevert <- function(SymbolNames, minDays, maxDays, trend=NULL)
{
  j <- 1
  lista <- list()
  names <- c()
  
  for(i in 1:length(SymbolNames))
  {
    reg <- findBestCurve(SymbolNames[i], minDays, maxDays)
    
    treg <- reg$regression
    
    dtrend <- revertTrend(treg, n=20)
      
    if( dtrend %in% trend)
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

filterIncomplete <- function(SymbolNames)
{
  symbols <- c()
  j <- 1
  for(i in 1:length(SymbolNames))
  {
    period <- sprintf("%s::%s", as.Date(Sys.Date() - 30 ), as.Date(Sys.Date()))
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

filterTrendLine <- function()
{
  
}
