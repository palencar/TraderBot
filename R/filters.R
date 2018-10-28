library("pastecs")
library("memoise")
library("xts")
library("quantmod")

lricache   <- new.env(hash=T, parent=emptyenv())

filterLRI <- function(SymbolName, tradeDate, n=30)
{
  alert <- NULL

  key <- paste0(SymbolName, " ", as.character(tradeDate))

  entry <- lricache[[key]]
  if(!is.null(entry))
  {
    return(as.character(entry))
  }

  obj <- base::get(SymbolName)[sprintf("/%s", tradeDate)]

  if(nrow(obj) <= n)
  {
    alert <- "none"
    lricache[[key]] <- alert
    return(alert)
  }

  lri <- na.omit(SMA(linearRegressionIndicator(SymbolName, obj, n), 10))

  if(is.null(lri))
  {
    alert <- "none"
    lricache[[key]] <- alert
    return(alert)
  }

  r <- rle(sign(diff(as.vector(lri))))

  len <- length(r$values)

  if(r$lengths[len] > 1 || len <= 3)
  {
    alert <- "none"
    lricache[[key]] <- alert
    return(alert)
  }

  alert <- "none"

  #if(r$values[len] == 1 && r$lengths[len] == 1)
  if(r$values[len] == 1)
  {
    alert <- "up"
  }

  #if(r$values[len] == -1 && r$lengths[len] == 1)
  if(r$values[len] == -1)
  {
    alert <- "down"
  }

  lricache[[key]] <- alert

  return(alert)
}

filterVolatility <- function(obj, symbol)
{
  if(nrow(obj) < 11)
  {
    print(sprintf("Insufficient data in: %s", symbol))
    return(NULL)
  }

  if(any(obj < 0))
  {
    print(sprintf("Adjust error: %s", symbol))
    return(NULL)
  }

  vol <- na.omit(diff(na.omit(volatility(obj))))

  if(max(abs(vol)) > 5)
  {
    print(sprintf("Probable adjust in %s: %s", symbol, paste(index(obj[which(na.omit(abs(diff(volatility(obj)))) > 5)]), collapse = " ")))
    return(NULL)
  }

  return(symbol)
}

#' @export
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

  for(symbol in SymbolNames)
  {
    obj <- tail(base::get(symbol)[sprintf("/%s", dateLimit)], 200)

    if(nrow(obj) < 10)
    {
      print(sprintf("NROW: %d", nrow(obj)))
      next
    }

    if(anyNA(obj))
    {
      print(sprintf("NA: %s", which(is.na(obj))))
      next
    }

    d <- diff(obj) == 0
    d[1,] <- FALSE
    rp <- mean(as.numeric(d))
    if(rp > 0.15)
    {
      print(sprintf("Repeated prices: %f", rp))
      next
    }

    symbols <- c(symbols, symbol)
  }

  exclude <- setdiff(SymbolNames, symbols)
  if(length(exclude) > 0)
  {
    print(sprintf("Bad Data Excluding [%s]: %s", dateLimit, paste(exclude, collapse = " ")))
  }

  return(symbols)
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
