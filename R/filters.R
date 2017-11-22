library("pastecs")
library("memoise")
library("xts")
library("quantmod")

lricache.l <- new.env(hash=T, parent=emptyenv())
lricache   <- new.env(hash=T, parent=emptyenv())

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

filterLRI <- function(SymbolName, tradeDate, threshold=0, n=30)
{
  alert <- NULL
  cacheName <- sprintf("datacache/lricache_%s_%1.2f.csv", SymbolName, threshold)

  key <- paste0(SymbolName, " ", as.character(tradeDate))

  envObj <- sprintf("%s_%1.2f_%d", SymbolName, threshold, n)
  if(is.null(lricache.l[[envObj]]))
  {
    if(file.exists(cacheName))
    {
      file.con <- file(cacheName, "r")
      table <- read.table(file.con, sep = ",", col.names = c("key", "alert"))
      close(file.con)

      for(i in 1:nrow(table))
        lricache[[as.character(table[i, "key"])]] <- table[i, "alert"]
    }

    lricache.l[[envObj]] <- TRUE
  }

  entry <- lricache[[key]]
  if(!is.null(entry))
  {
    return(as.character(entry))
  }

  lri <- linearRegressionIndicator(SymbolName, base::get(SymbolName)[sprintf("/%s", tradeDate)], n)[sprintf("/%s", tradeDate)]

  if(is.null(lri))
  {
    alert <- "none"
    file.con <- file(cacheName, "a")
    write.table(data.frame(key=key, alert=alert), file.con, row.names = F, na = "NA", append = T, quote = FALSE, sep=",", col.names=F)
    close(file.con)
    lricache[[key]] <- alert
    return(alert)
  }

  r <- rle(sign(diff(as.vector(lri))))

  len <- length(r$values)

  if(r$lengths[len] > 1 || len <= 3)
  {
    alert <- "none"
    file.con <- file(cacheName, "a")
    write.table(data.frame(key=key, alert=alert), file.con, row.names = F, na = "NA", append = T, quote = FALSE, sep=",", col.names=F)
    close(file.con)
    lricache[[key]] <- alert
    return(alert)
  }

  alert <- "none"

  if(r$values[len] == 1 && r$lengths[len] == 1)
  {
    alert <- "up"
  }

  if(r$values[len] == -1 && r$lengths[len] == 1)
  {
    alert <- "down"
  }

  file.con <- file(cacheName, "a")
  write.table(data.frame(key=key, alert=alert), file.con, row.names = F, na = "NA", append = T, quote = FALSE, sep=",", col.names=F)
  close(file.con)
  lricache[[key]] <- alert

  return(alert)
}

#' @export
filterData <- function(SymbolNames, endDate)
{
  toFilter <- filterVolume(SymbolNames, endDate)
  toFilter <- filterBadData(toFilter, endDate)

  return(toFilter)
}

#' @export
filterDataM <- memoise(function(SymbolNames, endDate)
{
  toFilter <- filterVolume(SymbolNames, endDate)
  toFilter <- filterBadData(toFilter, endDate)

  return(toFilter)
})

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
      warning(print(sprintf("Probable adjust in %s: %s", symbol, paste(index(obj[which(na.omit(abs(diff(volatility(obj)))) > 5)]), collapse = " "))))
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

#' @export
filterBadDataM <- memoise(filterBadData)

#' @export
filterVolume <- function(SymbolNames, dateLimit=NULL, age="1 year", volume = NULL)
{
  if(is.null(volume))
  {
    return(SymbolNames)
  }

  if(is.null(dateLimit) || is.na(dateLimit))
  {
    dateLimit <- Sys.time()
  }

  dt = dateLimit

  dc = sprintf("-%s", age)

  ds = seq(dt, length=2, by=dc)

  symbols <- NULL

  for(symb in SymbolNames)
  {
    period <- sprintf("%s::%s", ds[2], ds[1])

    obj <- base::get(symb)[period]

    vol <- as.double(Vo(obj))

    if(length(vol) < 200 || length(obj) < 500)
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
    paste0("Volume Excluding [", dt, "]: ", paste(exclude, collapse = " "))
  }

  return(symbols)
}

filterVolumeM <- memoise(filterVolume)

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

