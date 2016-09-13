source("dbInterface.R")

args <- commandArgs(trailingOnly=TRUE)

openTotalBuy <- 0
openTotalSell <- 0

closeTotalBuy <- 0
closeTotalSell <- 0

filePath <- "result"

if(length(args) >= 1)
  filePath <- args[1]  

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
          
          closeTotalBuy <- closeTotalBuy + buy_price
          closeTotalSell <- closeTotalSell + sell_price
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
      
      openTotalBuy <- openTotalBuy + buy_price
      openTotalSell <- openTotalSell + sell_price
    }
    positions <- NULL
    openDate <- NULL
  }
  closePosition <- FALSE
}

print(paste("Total open   ", openTotalBuy, openTotalSell, (openTotalSell - openTotalBuy), signif(((openTotalSell - openTotalBuy) / openTotalBuy), 2)))
print(paste("Total closed ", closeTotalBuy, closeTotalSell, (closeTotalSell - closeTotalBuy), signif(((closeTotalSell - closeTotalBuy) / closeTotalBuy), 2)))