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

  r <- RcppEigen::fastLm(y~x)

  yp <- predict(r)

  yr <- xts(yp, x)

  sigma <- summary(r)$sigma

  diffReg <- diff(yr)
  diffVal <- xts(y-yp, x)

  return(list(regression=yr, diffReg=diffReg, diffVal=diffVal, sigma=sigma, coef=r$coefficients["x"]))
}

linearRegressionIndicator <- function (SymbolName, Symbol, n=30, refresh = FALSE, cache = "file")
{
  dir.create("datacache", showWarnings=FALSE)

  fileName <- sprintf("datacache/%s_%d_lri.rds", SymbolName, n)
  memoryName <- sprintf("%s_%d_lri", SymbolName, n)

  lriFile <- NULL

  if(exists(memoryName) && refresh == FALSE)
    lriFile <- base::get(memoryName)

  if(is.null(lriFile) && cache == "file")
  {
    if(file.exists(fileName))
    {
      if(refresh)
      {
        file.remove(fileName)
        lriFile <- NULL
      }
      else
        lriFile <- readRDS(file=fileName)
    }
  }

  lastN <- (nrow(Symbol)-n)
  if(lastN <= 0)
  {
    return(NULL)
  }

  dolm <- function(x)
  {
    as.numeric(last(predict(RcppEigen::fastLm(formula = y~poly(x,2), data = as.data.frame(x)))))
  }

  dateInterval <- index(xts::last(Symbol, lastN))
  if(!is.null(lriFile))
  {
    idx <- dateInterval
    dateInterval <- dateInterval[!(dateInterval %in% index(lriFile))]
    dateInterval <- idx[idx >= first(dateInterval) & idx <= last(dateInterval)]

    lriFile <- lriFile[index(lriFile) %in% index(Symbol)]
  }

  if(length(dateInterval) > 0)
  {
    period <- paste0(index(xts::first(xts::last(Symbol[sprintf("/%s", first(dateInterval))], n))), "/", last(dateInterval))
    subsetSymbol <- Symbol[period]

    y <- as.double((Hi(subsetSymbol)+Lo(subsetSymbol)+Cl(subsetSymbol))/3)
    x <- 1:nrow(subsetSymbol)

    df <- data.frame(x, y)

    lri <- rollapplyr(df, n, dolm, by.column = FALSE)

    lriFile <- rbind(lriFile, xts(lri, dateInterval))
    lriFile <- lriFile[!duplicated(index(lriFile), fromLast = TRUE),]
    lriFile <- na.omit(lriFile)

    if(cache == "file")
      saveRDS(lriFile, file=fileName)
    if(cache == "memory")
      assign(memoryName, lriFile, envir = .GlobalEnv)
  }

  return(lriFile)
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

getLinRegOrders <- function(SymbolName, symbol, lri, threshold=0)
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
