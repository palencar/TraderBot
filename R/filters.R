library("pastecs")
library("memoise")
library("xts")
library("quantmod")
source("R/polyReg.R")
source("R/dbInterface.R")

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

  tp <- extract(turnpoints(sigmas), peak=0, pit=1)

  index <- which(tp==1)     #turnpints indexes

  return(object[which.min(sigmas[index])])  #which has minimal sigma
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

filterLRI <- function(SymbolName, tradeDate, threshold=0.6, n=30)
{
  alert <- NULL
  cacheName <- sprintf("datacache/lricache_%s_%1.2f.rds", SymbolName, threshold)

  key <- as.character(tradeDate)

  filterMap <- new.env(hash=T, parent=emptyenv())

  if(file.exists(cacheName))
  {
    filterMap <- readRDS(cacheName)
    if(!is.null(filterMap))
    {
      alert <- filterMap[[key]]
    }
  }

  if(!is.null(alert))
  {
    return(alert)
  }

  lri <- linearRegressionIndicator(SymbolName, get(SymbolName)[sprintf("/%s", tradeDate)], n)[sprintf("/%s", tradeDate)]

  if(is.null(lri))
  {
    return("none")
  }

  r <- rle(sign(diff(as.vector(lri))))

  len <- length(r$values)

  if(r$lengths[len] > 1 || len <= 3)
  {
    alert <- "none"
    filterMap[[key]] <- alert
    saveRDS(filterMap, file=cacheName)

    return(alert)
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

  sdev <- sd(rdif)
  lastSignal <- "none"

  for(i in 2:len)
  {
    if(r$values[i] == 1 && (rdif[i-1] <= (-sdev*threshold)))
    {
      lastSignal <- "up"
    }

    if(r$values[i] == -1 && (rdif[i-1] >= (sdev*threshold)))
    {
      lastSignal <- "down"
    }
  }

  alert <- "none"

  if(r$values[len] == 1 && (rdif[len-1] <= (-sdev*threshold) || lastSignal == "up"))
  {
    alert <- "up"
  }

  if(r$values[len] == -1 && (rdif[len-1] >= (sdev*threshold) || lastSignal == "down"))
  {
    alert <- "down"
  }

  filterMap[[key]] <- alert
  saveRDS(filterMap, file=cacheName)

  return(alert)
}

gapFree <- function(symbol, dateLimit, start=NULL)
{
  obj <- get(symbol)

  while(TRUE)
  {
    before   <- nrow(obj[sprintf("/%s", (dateLimit - 6))])
    interval <- nrow(obj[sprintf("%s/%s", (dateLimit - 5), dateLimit)])

    if(interval == 0)
    {
      if(before > 0)
      {
        warning(sprintf("excluding %s from symbols %s", symbol, sprintf("%s/%s", (dateLimit - 5), dateLimit)))
        return(FALSE)
      }
      else
      {
        return(TRUE)
      }
    }

    if(before == 0)
    {
      return(TRUE)
    }

    prevDates <- obj[sprintf("%s/%s", (dateLimit - 6), (dateLimit - 1))]
    if(length(prevDates) == 0)
    {
      return(FALSE)
    }

    dateLimit <- first(index(prevDates))
    if(is.na(dateLimit))
    {
      return(FALSE)
    }

    if(!is.null(start) && dateLimit <= start)
    {
      return(TRUE)
    }

    if(!is.null(start) && as.integer(dateLimit - start) < 1000)
    {
      return(gapFreeM(symbol, dateLimit, start))
    }
  }
}

gapFreeM <- memoise(gapFree)

#' @export
filterGap <- function(SymbolNames=NULL, dateLimit=NULL)
{
  if(is.null(SymbolNames))
  {
    return(NULL)
  }

  if(is.null(dateLimit))
  {
    dateLimit <- Sys.Date()
  }

  symbols <- NULL

  for(symbol in SymbolNames)
  {
    obj <- get(symbol)

    if(anyNA(OHLCV(obj)))
    {
      warning(sprintf("NA values for %s", symbol))
      next
    }

    if(gapFreeM(symbol, as.Date(dateLimit), as.Date(dateLimit) - 365))
    {
      symbols <- c(symbols, symbol)
    }
  }

  exclude <- setdiff(SymbolNames, symbols)
  if(length(exclude) > 0)
  {
    print(sprintf("Gap Excluding [%s]: %s", as.Date(dateLimit), paste(exclude, collapse = " ")))
  }

  return (symbols)
}

#' @export
filterGapM <- memoise(filterGap)

#' @export
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

#' @export
filterData <- function(SymbolNames, endDate)
{
  toFilter <- filterVolumeM(SymbolNames, endDate, volume = NULL)
  toFilter <- filterGapM(toFilter, endDate)
  toFilter <- filterBadDataM(toFilter, endDate)

  return(toFilter)
}

#' @export
filterDataM <- memoise(filterData)

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
    obj <- tail(get(symbol)[sprintf("/%s", dateLimit)], 200)

    if(nrow(obj) < 10)
    {
      warning(print(sprintf("NROW: %d", nrow(obj))))
      next
    }

    if(anyNA(obj))
    {
      warning(print(sprintf("NA: %s", which(is.na(obj)))))
      next
    }

    if(max(abs(na.omit(diff(volatility(obj))))) > 5)
    {
      warning(print(sprintf("Probable adjust in: %s", paste(obj[which(na.omit(diff(volatility(obj))) > 5)], collapse = " "))))
      next
    }

    symbols <- c(symbols, symbol)
  }

  exclude <- setdiff(SymbolNames, symbols)
  if(length(exclude) > 0)
  {
    print(sprintf("Bad Data Excluding [%s]: %s", as.Date(dateLimit), paste(exclude, collapse = " ")))
  }

  return(symbols)
}

#' @export
filterBadDataM <- memoise(filterBadData)

#' @export
filterVolume <- function(SymbolNames, dateLimit=NULL, age="1 year", volume = 400000)
{
  if(is.null(dateLimit) || is.na(dateLimit))
  {
    dateLimit <- Sys.Date()
  }

  dt = as.Date(dateLimit)

  dc = sprintf("-%s", age)

  ds = seq(dt, length=2, by=dc)

  symbols <- NULL

  for(symb in SymbolNames)
  {
    period <- sprintf("%s::%s", ds[2], ds[1])

    obj <- (get(symb)[period])

    vol <- as.double(Vo(obj))

    if(length(vol) < 200 || as.integer(as.Date(dt) - as.Date(first(index(get(symb))))) < 730)
    {
      next
    }

    meanVol <- as.double(mean(vol))

    if(is.null(meanVol) || !is.numeric(meanVol))
      next

    if(!is.null(volume) && meanVol < volume)
    {
      warning(sprintf("AVG Volume %s: %f < %f", symb, meanVol, volume))
      next
    }

    symbols <- c(symbols, symb)
  }

  exclude <- setdiff(SymbolNames, symbols)
  if(length(exclude) > 0)
  {
    print(sprintf("Volume Excluding [%s]: %s", as.Date(dt), paste(exclude, collapse = " ")))
  }

  return(symbols)
}

filterVolumeM <- memoise(filterVolume)

filterObjectsSets <- function(symbol, tradeDay)
{
  k1 <- 20
  k2 <- 100

  alerts <- NULL
  cacheFile <- NULL
  regset <- NULL
  turnpoints_r <- list()

  filterMap <- new.env(hash=T, parent=emptyenv())

  key <- as.character(tradeDay)

  cacheName <- sprintf("datacache/turncache_%s_%d_%d.rds", symbol, k1, k2)
  if(file.exists(cacheName))
  {
    filterMap <- readRDS(cacheName)
    if(!is.null(filterMap))
    {
      alerts <- filterMap[[key]]
    }
  }

  if(is.null(alerts))
  {
    regset <- findCurves(symbol, k1, k2, tradeDay)

    if(is.null(regset))
    {
      warning("regset = NULL")
      return(NULL)
    }

    if(length(regset) == 0)
    {
      warning("length(regset) = 0")
      return(NULL)
    }

    if(length(get(symbol)[tradeDay]) == 0)
    {
      warning(sprintf("no data for %s", tradeDay))
      return(NULL)
    }

    alertas = tryCatch({
      turnPoints(regset)
    }, warning = function(war) {
      print(war)
      print(sprintf("%s %s", symbol, tradeDay))
      return(NULL)
    }, error = function(err) {
      print(err)
      print(sprintf("%s %s", symbol, tradeDay))
      return(NULL)
    }, finally={
    })

    trend <- c("r_up")
    turnpoints_r$r_up <- filterRevert(alertas, trend, 3)

    if(length(turnpoints_r$r_up) > 0)
    {
      objectName_ru <- sprintf("datacache/%s-%s_%d_%d_turnpoints_r_up.rds", tradeDay, symbol, k1, k2)

      if((trend %in% alerts) == FALSE)
      {
        alerts <- c(alerts, trend)
      }

      saveRDS(turnpoints_r$r_up, file=objectName_ru)
    }

    trend <- c("r_down")
    turnpoints_r$r_down <- filterRevert(alertas, trend, 3)

    if(length(turnpoints_r$r_down) > 0)
    {
      objectName_rd <- sprintf("datacache/%s-%s_%d_%d_turnpoints_r_down.rds", tradeDay, symbol, k1, k2)

      if((trend %in% alerts) == FALSE)
      {
        alerts <- c(alerts, trend)
      }

      saveRDS(turnpoints_r$r_down, file=objectName_rd)
    }

    if(is.null(alerts))
    {
      alerts <- FALSE
    }

    filterMap[[key]] <- alerts
    saveRDS(filterMap, cacheName)
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
