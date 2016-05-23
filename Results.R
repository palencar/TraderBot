source("dbInterface.R")

filePath <- "training"

objFiles <- list.files(filePath, pattern="*log")

for(logFile in objFiles)
{
  lines = readLines(sprintf("%s/%s", filePath, logFile))
  
  positions <- NULL
  openDate <- NULL
  closePosition <- FALSE
  
  for(line in lines)
  {
    elements <- unlist(strsplit(line, " "))
    
    if(elements[3] == "sell")
    {
      if(is.null(positions) == FALSE)
      {
        i <- 1
        for(position in positions)
        {
          sell_price <- as.integer(as.double(elements[4])*100)
          buy_price <- as.integer(position*100)
          print(paste("closed", elements[1], buy_price, sell_price, (sell_price - buy_price), signif(((sell_price - buy_price) / buy_price), 2), openDate[i], elements[2]))
          i <- i + 1
        }
        positions <- NULL
        openDate <- NULL
        closePosition <- TRUE
      }
    }
        
    if(elements[3] == "buy")
    {
      positions <- c(positions, as.double(elements[4]))
      openDate <- c(openDate, elements[2])
    }
  }
  
  if(closePosition == FALSE)
  {
    lastDay <- lastTradeDay(elements[1])
    i <- 1
    for(position in positions)
    {
      sell_price <- as.integer(lastPrice(elements[1])*100)
      buy_price <- as.integer(position*100)
      print(paste("open  ", elements[1], buy_price, sell_price, (sell_price - buy_price), signif(((sell_price - buy_price) / buy_price), 2), openDate[i], lastDay))
      i <- i + 1
    }
    positions <- NULL
    openDate <- NULL
  }
  closePosition <- FALSE
}

