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

linearRegressionIndicator <- function (SymbolName, Symbol, window=720, n=30)
{
  require(quantmod)
  
  fileName <- sprintf("data/%s_%d_lri.rds", SymbolName, n)
  
  fileExists <- file.exists(fileName)
  
  lastDate  <- as.Date(xts::last(index(Symbol)))
  
  if(fileExists)
  {
    lriFile <- readRDS(file=fileName)
    
    firstDate <- as.Date(xts::last(index(lriFile)))
  }
  else
  {
    firstDate <- as.Date(lastDate-window)
  }
  
  if(firstDate < as.Date(xts::first(index(Symbol))))
    return(NULL)
    #firstDate <- as.Date(xts::first(index(Symbol)))
  
  if(firstDate > lastDate)
    firstDate <- as.Date(lastDate-window)
  
  dateInterval <- Symbol[sprintf("%s/%s", firstDate, lastDate)] 
  
  lri <- c()
  
  for(i in 1:nrow(dateInterval))
  {
    xDate <- as.Date(index(dateInterval[i]))
    
    startDate <- as.Date(xDate-n)
    endDate   <- as.Date(xDate)
    subsetSymbol <- Symbol[sprintf("%s::%s", startDate, endDate)]

    if(nrow(subsetSymbol) < 3)
    {
      warning(sprintf("%s %s/%s", SymbolName, startDate, endDate))
      return(NULL)
    }
    
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
  
  if(fileExists)
  {
    xi <- rbind(lriFile[sprintf("::%s", (as.Date(index(last(lriFile)))-1))], xts(lri, index(dateInterval)))
  }
  else
  {
    xi <- xts(lri, index(dateInterval))
  }

  xi<-xi[!duplicated(index(xi)),]
  
  saveRDS(xi, file=fileName)
  
  xi <- DEMA(xi, n = 10)
  xi <- xi[!is.na(xi),]
  
  return(xi)
}

getLinRegIndicators <- function(SymbolName, Symbol, n=c(30))
{
  lri <- NULL
    
  for(i in n)
  {
    objName <- sprintf("lri%s.p%d", SymbolName, i)
    obj <- linearRegressionIndicator(SymbolName, Symbol, n=i)
    
    if(is.null(obj))
      next
    
    assign(objName, obj, .GlobalEnv)
    lri <- c(lri, sprintf("addTA(%s, on=1, col=3)", objName))
  }
    
  return(lri)
}

getLinRegOrders <- function(SymbolName, symbol, lri, threshold=0.6)#TODO unificar esta funcao com filterLRI
{
  if(is.null(lri))
  {
    return(NULL)
  }
  
  r <- rle(sign(diff(as.vector(lri))))
  
  len <- length(r$values)
  
  if(len <= 3)
  {
    return(NULL)
  }
  
  rdif <- c()
  
  indexes <- c()
  
  lastIndex <- 1
  for(i in 1:len)
  {
    indexes[i] <- lastIndex
    nextIndex <- lastIndex + r$lengths[i]
    
    tryCatch({
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
    }, warning = function(war) {
      print(war)
      return(Inf)
    }, error = function(err) {
      print(err)
      return(Inf)
    }, finally={
    })
    
    lastIndex <- nextIndex
  }
  
  sdev <- sd(rdif)
  
  signals <- NULL
  lastSignal <- "none"
  
  longSignal <- NULL
  shortSignal <- NULL
  
  for(i in 2:len)
  {
    if(r$values[i] == 1 && (rdif[i-1] <= (-sdev*threshold) || lastSignal == "blue"))
    {
      bluePoint <- xts(as.double(lri[indexes[i]]), as.Date(index(lri[indexes[i]])))
      if(is.null(longSignal))
        longSignal <- bluePoint
      else
        longSignal <- rbind(longSignal, bluePoint)
      lastSignal <- "blue"
    }
    
    if(r$values[i] == -1 && (rdif[i-1] >= (sdev*threshold) || lastSignal == "red"))
    {
      redPoint <- xts(as.double(lri[indexes[i]]), as.Date(index(lri[indexes[i]])))
      if(is.null(shortSignal))
        shortSignal <- redPoint
      else
        shortSignal <- rbind(shortSignal, redPoint)
      lastSignal <- "red"
    }
  }
  
  if(is.null(longSignal) == FALSE)
  {
    objName <- sprintf("lriLong%s", SymbolName)
    signals <- c(signals, sprintf("addTA(%s, on = 1, col = 'blue', type = 'p', lwd = 1, pch=19)", objName))
    assign(objName, longSignal, .GlobalEnv)
  }
  
  if(is.null(shortSignal) == FALSE)
  {
    objName <- sprintf("lriShort%s", SymbolName)
    signals <- c(signals, sprintf("addTA(%s, on = 1, col = 'red', type = 'p', lwd = 1, pch=19)", objName))
    assign(objName, shortSignal, .GlobalEnv)
  }
  
  return(signals)
}