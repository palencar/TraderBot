library(data.table)
source("dbInterface.R")

args <- commandArgs(trailingOnly=TRUE)

openTotalBuy <- 0
openTotalSell <- 0

closeTotalBuy <- 0
closeTotalSell <- 0

filePath <- "result"

showAll <- FALSE
report <- FALSE

if(length(args) >= 2 && "--all" %in% args)
  showAll <- TRUE

if(length(args) >= 2 && "--report" %in% args)
  report <- TRUE

if(length(args) >= 1)
  filePath <- args[length(args)]

singleResult <- function(key, objFiles)
{
  closedDF <- NULL
  openDF <- NULL
  
  #print(key)
  #print(objFiles)
  
  if(length(objFiles) == 0)
  {
    return(NULL)
  }
  
  for(logFile in objFiles)
  {
    obj <- get(logFile)
    lines <- obj[[key]]
    
    #print(sprintf("%s %d", logFile, length(lines)))
    
    if(is.null(lines))
      next
    
    positions <- NULL
    openDate <- NULL
    closePosition <- FALSE
    
    lines <- strsplit(lines, " ")
    
    for(elements in lines)
    {
      if(elements[3] == "sell")
      {
        if(is.null(positions) == FALSE)
        {
          i <- 1
          for(position in positions)
          {
            sell_price <- as.integer(as.double(elements[4])*100)
            buy_price <- as.integer(position*100)
            
            newrow <- data.frame("closed", elements[1], buy_price, sell_price, (sell_price - buy_price), signif(((sell_price - buy_price) / buy_price), 2), openDate[i], elements[2])
            closedDF <- rbind(closedDF, newrow)
            
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
        
        newrow <- data.frame("open", elements[1], buy_price, sell_price, (sell_price - buy_price), signif(((sell_price - buy_price) / buy_price), 2), openDate[i], lastDay)
        openDF <- rbind(openDF, newrow)
        
        i <- i + 1
      }
      positions <- NULL
      openDate <- NULL
    }
    closePosition <- FALSE
  }
  
  colNames <- c("state", "name", "buy_price", "sell_price", "profit", "proffit_pp", "open", "last")
  
  if(!is.null(closedDF))
  {
    colnames(closedDF) <- colNames
    closedDF <- closedDF[order(closedDF$proffit_pp),]

    if(report == FALSE)
    {
      if(showAll)
        print(closedDF)
      print(sprintf("Total closed: %d %d %.2f", sum(closedDF$buy_price), sum(closedDF$sell_price-closedDF$buy_price), sum(closedDF$sell_price-closedDF$buy_price)/sum(closedDF$buy_price)))
    }
  }
  
  if(!is.null(openDF))
  {
    colnames(openDF) <- colNames
    openDF <- openDF[order(openDF$proffit_pp),]
    
    if(report == FALSE)
    {
      if(showAll)
        print(openDF)
      print(sprintf("Total open  : %d %d %.2f", sum(openDF$buy_price), sum(openDF$sell_price-openDF$buy_price), sum(openDF$sell_price-openDF$buy_price)/sum(openDF$buy_price)))
    }
  }
  
  totalDF <- rbind(openDF, closedDF)
  
  if(report)
  {
    if(!is.null(totalDF$buy_price))
    {
      strOut <- sprintf("%s %d %d %.2f", key, sum(totalDF$buy_price), sum(totalDF$sell_price-totalDF$buy_price), sum(totalDF$sell_price-totalDF$buy_price)/sum(totalDF$buy_price))
      cat(strOut, sep = "\n")
    }
  }
  else
  {
    if(sum(totalDF$buy_price) > 0)
    {
      print(sprintf("Total       : %d %d %.2f", sum(totalDF$buy_price), sum(totalDF$sell_price-totalDF$buy_price), sum(totalDF$sell_price-totalDF$buy_price)/sum(totalDF$buy_price)))
    }
  }
}

sink(sprintf("%s.txt", filePath))
files <- list.files(filePath, pattern="*.rds")

for(name in files)
{
  obj <- readRDS(sprintf("%s/%s", filePath, name))
  assign(name, obj)
  
  pars <- unique(ls(obj))

  for(key in pars)
  {
    singleResult(key, name)
  }
}

sink()
