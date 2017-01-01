library("data.table")
library("memoise")
library("moments")
source("R/dbInterface.R")

mergeBacktest_ <- function(path = "result")
{
  files <- list.files(path, pattern = "*.txt")

  data <- list()
  i <- 1

  for(file in files)
  {
    name <- paste(path, file, sep = "/")
    obj <- data.frame(read.table(name, sep = " "))
    obj$symbol <- unlist(strsplit(file, "[.]"))[1]
    data[[i]] <- obj
    i <- i + 1
  }

  dataTable <- rbindlist(data)

  return(dataTable)
}

mergeBacktest <- memoise(mergeBacktest_)

showSmaPeriod <- function(dataTable)
{
  df <- data.frame(dataTable$V1, dataTable$V11)
  df <- unique(df[complete.cases(df),])
  colnames(df) <- c("smaPeriod", "proffit")

  scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
}

showUpperBand <- function(dataTable)
{
  df <- data.frame(dataTable$V2, dataTable$V11)
  df <- unique(df[complete.cases(df),])
  colnames(df)  <- c("upperBand", "proffit")

  scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
}

showLowerBand <- function(dataTable)
{
  df <- data.frame(dataTable$V3, dataTable$V11)
  df <- unique(df[complete.cases(df),])
  colnames(df)  <- c("lowerBand", "proffit")

  scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
}

showDownChange <- function(dataTable)
{
  df <- data.frame(dataTable$V4, dataTable$V11)
  df <- unique(df[complete.cases(df),])
  colnames(df)  <- c("downChange", "proffit")

  scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
}

showUpChange <- function(dataTable)
{
  df <- data.frame(dataTable$V5, dataTable$V11)
  df <- unique(df[complete.cases(df),])
  colnames(df)  <- c("upChange", "proffit")

  scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
}

showLowerLimit <- function(dataTable)
{
  df <- data.frame(dataTable$V6, dataTable$V11)
  df <- unique(df[complete.cases(df),])
  colnames(df)  <- c("lowerLimit", "proffit")

  scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
}

showStopGain <- function(dataTable)
{
  df <- data.frame(dataTable$V7, dataTable$V11)
  df <- unique(df[complete.cases(df),])
  colnames(df)  <- c("stopGain", "proffit")

  scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
}

showStopLoss <- function(dataTable)
{
  df <- data.frame(dataTable$V8, dataTable$V11)
  df <- unique(df[complete.cases(df),])
  colnames(df)  <- c("stopLoss", "proffit")

  scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
}

showReport <- function(dataTable, path = "result")
{
  dff <- NULL
  i <- 1

  symbols <- unique(dataTable$symbol)

  if(length(symbols) > 0)
  {
    symbols <- startProbe(symbolNames = symbols, minAge=400, update=FALSE)
  }

  for(symbolName in symbols)
  {
    obj        <- dataTable[which(dataTable$symbol == symbolName)]
    proffit    <- mean(obj$V11)
    minProffit <- min(obj$V11)
    maxProffit <- max(obj$V11)
    skewness   <- skewness(obj$V11)
    volatility <- mean(na.omit(volatility(get(symbolName))))
    volume     <- mean(as.numeric(na.omit(Vo(get(symbolName)))))
    df         <- data.frame(symbolName, proffit, minProffit, maxProffit, skewness, volatility, volume)
    dff        <- rbind(dff, df)
  }

  dff <- dff[order(dff$proffit),]

  return(dff)
}


#qplot(factor(symbol), V11, data = dataTable, geom = "boxplot")
