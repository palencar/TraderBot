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

    if(is.null(obj))
      next

    symbol <- unlist(strsplit(file, "[._]"))[1]
    timeframe <- unlist(strsplit(file, "[._]"))[2]
    oper[[i]] <- data.frame(symbol, timeframe, obj$operations)
    i <- i + 1
  }

  dataTable <- rbindlist(oper, fill = TRUE)

  if(nrow(dataTable) > 0)
    dataTable$mProfit <- as.numeric(dataTable$profit_pp)/(as.numeric(difftime(dataTable$last, dataTable$open), units = "days")/30)

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
    profit     <- mean(obj$profit_pp)
    minProfit  <- min(obj$profit_pp)
    maxProfit  <- max(obj$profit_pp)
    variance   <- var(obj$profit_pp)
    skewness   <- skewness(obj$profit_pp)
    count      <- length(unique(obj$profit_pp[obj$name == symbolName]))
    df         <- data.frame(symbolName, profit, minProfit, maxProfit, variance, skewness, count)
    dff        <- rbind(dff, df)
  }

  dff <- dff[order(dff$profit),]

  return(dff)
}
