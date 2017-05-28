library("RcppEigen")
library("quantmod")
library("xts")

linearRegression <- function (Symbol)
{
  if(is.HLC(Symbol))
  {
    y <- as.double((Hi(Symbol)+Lo(Symbol)+Cl(Symbol))/3)
  }
  else
  {
    y <- as.double(Symbol[,1])
  }

  x <- index(Symbol)

  r <- fastLm(y~x)

  yp <- predict(r)

  yr <- xts(yp, x)

  sigma <- summary(r)$sigma

  diffReg <- diff(yr)
  diffVal <- xts(y-yp, x)

  return(list(regression=yr, diffReg=diffReg, diffVal=diffVal, sigma=sigma, coef=r$coefficients["x"]))
}

linearRegressionIndicator <- function (SymbolName, Symbol, window=720, n=30)
{
  update <- FALSE

  fileName <- sprintf("datacache/%s_%d_lri.rds", SymbolName, n)

  fileExists <- file.exists(fileName)

  lriFile <- NULL
  if(fileExists)
  {
    lriFile <- readRDS(file=fileName)
  }

  lastN <- (nrow(Symbol)-n)
  if(lastN <= 0)
  {
    return(NULL)
  }

  dateInterval <- index(xts::last(Symbol, lastN))
  if(!is.null(lriFile))
  {
    dateInterval <- dateInterval[!(dateInterval %in% (index(lriFile)))] #TODO atualizar o ultimo
  }

  lri <- c()

  if(length(dateInterval) > 0)
  {
    update <- TRUE
  }

  if(update)
  {
    for(i in 1:length(dateInterval))
    {
      endDateTime <- dateInterval[i]
      startDateTime <- index(xts::first(xts::last(Symbol[sprintf("/%s",endDateTime)], n)))

      subsetSymbol <- Symbol[sprintf("%s/%s", startDateTime, endDateTime)]

      if(nrow(subsetSymbol) < 3)
      {
        next
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

      x <- index(subsetSymbol)

      lri[i] <- as.numeric(last(predict(lm(y~poly(x,2)))))
    }
  }

  if(fileExists)
  {
    xi <- rbind(lriFile[sprintf("::%s", (index(xts::last(lriFile)))-1)], xts(lri, dateInterval))
  }
  else
  {
    xi <- xts(lri, dateInterval)
  }

  if(update)
  {
    xi<-xi[!duplicated(index(xi)),]
    xi <- xi[!is.na(xi),]
    saveRDS(xi, file=fileName)
  }

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

  sdev <- sd(rdif)

  signals <- NULL
  lastSignal <- "none"

  longSignal <- NULL
  shortSignal <- NULL

  for(i in 2:len)
  {
    if(r$values[i] == 1 && (rdif[i-1] <= (-sdev*threshold) || lastSignal == "blue"))
    {
      bluePoint <- xts(as.double(lri[indexes[i]]), index(lri[indexes[i]]))
      if(is.null(longSignal))
        longSignal <- bluePoint
      else
        longSignal <- rbind(longSignal, bluePoint)
      lastSignal <- "blue"
    }

    if(r$values[i] == -1 && (rdif[i-1] >= (sdev*threshold) || lastSignal == "red"))
    {
      redPoint <- xts(as.double(lri[indexes[i]]), index(lri[indexes[i]]))
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
