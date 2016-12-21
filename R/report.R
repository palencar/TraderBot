library("data.table")
library("memoise")
source("R/dbInterface.R")

mergeBacktest_ <- function(path = "result")
{
  files <- list.files(path, pattern = "*.txt")

  data <- list()
  i <- 1

  for(file in files)
  {
    #print(file)
    name <- paste(path, file, sep = "/")
    obj <- data.frame(read.table(name, sep = " "))
    obj$symbol <- unlist(strsplit(file, "[.]"))[1]
    data[[i]] <- tail(obj, n=40)
    i <- i + 1
  }

  dataTable <- rbindlist(data)

  return(dataTable)
}

mergeBacktest <- memoise(mergeBacktest_)

filterBacktest <- function(dataTable, limits = NULL)
{
  if(is.null(limits))
  {
    return(dataTable)
  }

  dataTable <- dataTable[which(dataTable$V1) >= limits$minV1]
  dataTable <- dataTable[which(dataTable$V1) <= limits$maxV1]

  dataTable <- dataTable[which(dataTable$V1) >= limits$minV2]
  dataTable <- dataTable[which(dataTable$V1) <= limits$maxV2]

  dataTable <- dataTable[which(dataTable$V1) >= limits$minV3]
  dataTable <- dataTable[which(dataTable$V1) <= limits$maxV3]

  dataTable <- dataTable[which(dataTable$V1) >= limits$minV4]
  dataTable <- dataTable[which(dataTable$V1) <= limits$maxV4]

  dataTable <- dataTable[which(dataTable$V1) >= limits$minV5]
  dataTable <- dataTable[which(dataTable$V1) <= limits$maxV5]

  dataTable <- dataTable[which(dataTable$V1) >= limits$minV6]
  dataTable <- dataTable[which(dataTable$V1) <= limits$maxV6]

  dataTable <- dataTable[which(dataTable$V1) >= limits$minV7]
  dataTable <- dataTable[which(dataTable$V1) <= limits$maxV7]

  return(dataTable)
}

showSmaPeriod <- function()
{
  dataTable <- mergeBacktest()

  df <- data.frame(dataTable$V1, dataTable$V11)
  df <- unique(df[complete.cases(df),])
  colnames(df) <- c("smaPeriod", "proffit")

  scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
}

showUpperBand <- function()
{
  dataTable <- mergeBacktest()

  df <- data.frame(dataTable$V2, dataTable$V11)
  df <- unique(df[complete.cases(df),])
  colnames(df)  <- c("upperBand", "proffit")

  scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
}

showLowerBand <- function()
{
  dataTable <- mergeBacktest()

  df <- data.frame(dataTable$V3, dataTable$V11)
  df <- unique(df[complete.cases(df),])
  colnames(df)  <- c("lowerBand", "proffit")

  scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
}

showDownChange <- function()
{
  dataTable <- mergeBacktest()

  df <- data.frame(dataTable$V4, dataTable$V11)
  df <- unique(df[complete.cases(df),])
  colnames(df)  <- c("downChange", "proffit")

  scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
}

showUpChange <- function()
{
  dataTable <- mergeBacktest()

  df <- data.frame(dataTable$V5, dataTable$V11)
  df <- unique(df[complete.cases(df),])
  colnames(df)  <- c("upChange", "proffit")

  scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
}

showLowerLimit <- function()
{
  dataTable <- mergeBacktest()

  df <- data.frame(dataTable$V6, dataTable$V11)
  df <- unique(df[complete.cases(df),])
  colnames(df)  <- c("lowerLimit", "proffit")

  scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
}

showStopGain <- function()
{
  dataTable <- mergeBacktest()

  df <- data.frame(dataTable$V7, dataTable$V11)
  df <- unique(df[complete.cases(df),])
  colnames(df)  <- c("stopGain", "proffit")

  scatter.smooth(df, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
}

showStopLoss <- function()
{
  dataTable <- mergeBacktest()

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
    volatility <- mean(na.omit(volatility(get(symbolName))))
    volume     <- mean(as.numeric(na.omit(Vo(get(symbolName)))))
    df         <- data.frame(symbolName, proffit, volatility, volume)
    dff        <- rbind(dff, df)
  }

  dff <- dff[order(dff$proffit),]

  return(dff)
}
