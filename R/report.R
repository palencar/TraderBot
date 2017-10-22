library("data.table")
library("memoise")
library("moments")
source("R/dbInterface.R")

mergeBacktest <- function(path = "result")
{
  files <- list.files(path, pattern = "*.rds")

  oper <- list()
  i <- 1

  for(file in files)
  {
    name   <- paste(path, file, sep = "/")
    obj    <- readRDS(name)
    symbol <- unlist(strsplit(file, "[._]"))[1]
    timeframe <- unlist(strsplit(file, "[._]"))[2]
    oper[[i]] <- data.frame(symbol, timeframe, obj$operations)
    i <- i + 1
  }

  dataTable <- rbindlist(oper, fill = TRUE)

  if(nrow(dataTable) > 0)
    dataTable$mProffit <- as.numeric(dataTable$proffit_pp)/(as.numeric(difftime(dataTable$last, dataTable$open), units = "days")/30)

  return(dataTable)
}

showPlot <- function(dataTable, xy)
{
  df <- data.frame(dataTable)
  df <- df[c(xy, 'timeframe')]
  df <- unique(df[complete.cases(df),])

  if(nrow(df) > 0)
  {
    ggplot(df, aes_string(x = xy[1], y = xy[2], color='timeframe')) + geom_point(alpha=0.05) + geom_smooth()
  }
}

showReport <- function(dataTable, path = "result")
{
  dff <- NULL
  i <- 1

  symbols <- unique(as.vector(dataTable$name))

  for(symbolName in symbols)
  {
    obj        <- dataTable[which(dataTable$name == symbolName)]
    proffit    <- mean(obj$proffit_pp)
    minProffit <- min(obj$proffit_pp)
    maxProffit <- max(obj$proffit_pp)
    variance   <- var(obj$proffit_pp)
    skewness   <- skewness(obj$proffit_pp)
    count      <- length(unique(obj$proffit_pp[obj$name == symbolName]))
    df         <- data.frame(symbolName, proffit, minProffit, maxProffit, variance, skewness, count)
    dff        <- rbind(dff, df)
  }

  dff <- dff[order(dff$proffit),]

  return(dff)
}
