source("dbInterface.R")

filePath <- "training"

objFiles <- list.files(filePath, pattern="*log")

for(logFile in objFiles)
{
  lines = readLines(sprintf("%s/%s", filePath, logFile))
  
  positions <- NULL
  openDate <- NULL
  
  for(line in lines)
  {
    elements <- unlist(strsplit(line, " "))
    
    if(elements[3] == "sell")
    {
      if(is.null(positions) == FALSE)
      {
        sell_price <- (length(positions)*as.double(elements[4])*100)
        buy_price <- (sum(positions)*100)
        print(paste("closed", elements[1], signif((sell_price - buy_price), 2), signif((100*length(positions)), 2), signif(((sell_price - buy_price) / buy_price), 2)))
        #print(paste(openDate, elements[2], collapse = " "))
        positions <- NULL
        openDate <- NULL
      }
    }
        
    if(elements[3] == "buy")
    {
      positions <- c(positions, as.double(elements[4]))
      openDate <- c(openDate, elements[2])
    }
  }
  
  if(is.null(positions) == FALSE)
  {
    sell_price <- (length(positions)*as.double(lastPrice(elements[1]))*100)
    buy_price <- (sum(positions)*100)
    print(paste("open  ", elements[1], signif((sell_price - buy_price), 2), signif((100*length(positions)), 2), signif(((sell_price - buy_price) / buy_price), 2)))
    #print(paste(openDate, elements[2], collapse = " "))
    positions <- NULL
    openDate <- NULL
  }
}

