library("data.table")
library("memoise")
library("moments")
source("R/dbInterface.R")

mergeBacktest <- function(path = "result")
{
  objName <- "backtest"

  if(exists(objName) && (Sys.time() < base::get(objName)$datetime + 600))
    return(base::get(objName)$dataTable)

  files <- list.files(path, pattern = "*.rds")

  data <- list()
  i <- 1

  for(file in files)
  {
    name   <- paste(path, file, sep = "/")
    obj    <- readRDS(name)
    symbol <- unlist(strsplit(file, "[._]"))[1]
    data[[i]] <- data.frame(symbol, obj$parameters, obj$results)
    i <- i + 1
  }

  dataTable <- rbindlist(data, fill = TRUE)

  obj <- c()
  obj$datetime <- Sys.time()
  obj$dataTable <- dataTable
  assign(objName, obj, .GlobalEnv)

  return(dataTable)
}

showSmaPeriod <- function(dataTable)
{
  df <- data.frame(dataTable)
  df <- df[c("smaPeriod", "proffit")]
  df <- unique(df[complete.cases(df),])

  scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
}

showUpperBand <- function(dataTable)
{
  df <- data.frame(dataTable)
  df <- df[c("upperBand", "proffit")]
  df <- unique(df[complete.cases(df),])

  scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
}

showLowerBand <- function(dataTable)
{
  df <- data.frame(dataTable)
  df <- df[c("lowerBand", "proffit")]
  df <- unique(df[complete.cases(df),])

  scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
}

showDownChange <- function(dataTable)
{
  df <- data.frame(dataTable)
  df <- df[c("downChange", "proffit")]
  df <- unique(df[complete.cases(df),])

  scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
}

showUpChange <- function(dataTable)
{
  df <- data.frame(dataTable)
  df <- df[c("upChange", "proffit")]
  df <- unique(df[complete.cases(df),])

  scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
}

showLowerLimit <- function(dataTable)
{
  df <- data.frame(dataTable)
  df <- df[c("lowLimit", "proffit")]
  df <- unique(df[complete.cases(df),])

  if(nrow(df) > 0)
  {
    scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
  }
}

showStopGain <- function(dataTable)
{
  df <- data.frame(dataTable)
  df <- df[c("stopGain", "proffit")]
  df <- unique(df[complete.cases(df),])

  if(nrow(df) > 0)
  {
    scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
  }
}

showStopLoss <- function(dataTable)
{
  df <- data.frame(dataTable)
  df <- df[c("stopLoss", "proffit")]
  df <- unique(df[complete.cases(df),])

  if(nrow(df) > 0)
  {
    scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
  }
}

showBullish <- function(dataTable)
{
  df <- data.frame(dataTable)
  df <- df[c("bullish", "proffit")]
  df <- unique(df[complete.cases(df),])

  if(nrow(df) > 0)
  {
    scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
  }
}

showBearish <- function(dataTable)
{
  df <- data.frame(dataTable)
  df <- df[c("bearish", "proffit")]
  df <- unique(df[complete.cases(df),])

  if(nrow(df) > 0)
  {
    scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
  }
}

showReport <- function(dataTable, path = "result")
{
  dff <- NULL
  i <- 1

  symbols <- unique(as.vector(dataTable$symbol))

  if(length(symbols) > 0)
  {
    symbols <- startProbe(symbolNames = symbols, minAge=400, update=FALSE)
  }

  for(symbolName in symbols)
  {
    obj        <- dataTable[which(dataTable$symbol == symbolName)]
    proffit    <- mean(obj$proffit)
    minProffit <- min(obj$proffit)
    maxProffit <- max(obj$proffit)
    variance   <- var(obj$proffit)
    skewness   <- skewness(obj$proffit)
    count      <- length(unique(obj$proffit[obj$symbol == symbolName]))
    volatility <- mean(na.omit(volatility(base::get(symbolName))))
    volume     <- mean(as.numeric(na.omit(Vo(base::get(symbolName)))))
    df         <- data.frame(symbolName, proffit, minProffit, maxProffit, variance, skewness, count, volatility, volume)
    dff        <- rbind(dff, df)
  }

  dff <- dff[order(dff$proffit),]

  return(dff)
}

#qplot(factor(symbol), proffit, data = dataTable, geom = "boxplot")
