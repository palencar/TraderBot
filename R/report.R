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

  oper <- list()
  i <- 1

  for(file in files)
  {
    name   <- paste(path, file, sep = "/")
    obj    <- readRDS(name)
    symbol <- unlist(strsplit(file, "[._]"))[1]
    oper[[i]] <- data.frame(symbol, obj$operations)
    i <- i + 1
  }

  dataTable <- rbindlist(oper, fill = TRUE)
  dataTable$mProffit <- as.numeric(dataTable$proffit_pp)/(as.numeric(difftime(dataTable$last, dataTable$open), units = "days")/30)

  obj <- c()
  obj$datetime <- Sys.time()
  obj$dataTable <- dataTable
  assign(objName, obj, .GlobalEnv)

  return(dataTable)
}

showPlot <- function(dataTable, xy)
{
  df <- data.frame(dataTable)
  df <- df[xy]
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
    symbols <- getSymbolsDaily(symbolNames = symbols)
  }

  for(symbolName in symbols)
  {
    obj        <- dataTable[which(dataTable$symbol == symbolName)]
    proffit    <- mean(obj$proffit_pp)
    minProffit <- min(obj$proffit_pp)
    maxProffit <- max(obj$proffit_pp)
    variance   <- var(obj$proffit_pp)
    skewness   <- skewness(obj$proffit_pp)
    count      <- length(unique(obj$proffit_pp[obj$symbol == symbolName]))
    volatility <- mean(na.omit(volatility(base::get(symbolName))))
    volume     <- mean(as.numeric(na.omit(Vo(base::get(symbolName)))))
    df         <- data.frame(symbolName, proffit, minProffit, maxProffit, variance, skewness, count, volatility, volume)
    dff        <- rbind(dff, df)
  }

  dff <- dff[order(dff$proffit),]

  return(dff)
}
