library("memoise")

writeResult <- function(symbol, result, parameters = NULL)
{
  resultPath <- "result"
  dir.create(resultPath, showWarnings=FALSE)

  if(!is.null(parameters))
  {
    resultPath <- sprintf("%s/%s", resultPath, paste(parameters, collapse = ""))
  }
  else
  {
    resultPath <- sprintf("%s/default", resultPath)
  }
  
  dir.create(resultPath, showWarnings=FALSE)

  logFile <- paste(resultPath,"/",symbol,".log", sep = "")
  
  lines <- NULL
  if(file.exists(logFile))
  {
    lines <- readLines(logFile)
  }
  
  if((result %in% lines) == FALSE)
  {
    if(is.null(lines) || lines[length(lines)] < result)
    {
      cat(result, file=logFile, sep = "\n", append=TRUE)
    }
    else if(!(result %in% lines))
    {
      lines <- sort(unique(c(lines, result)))
      fileConn <- file(logFile)
      writeLines(lines, fileConn)
      close(fileConn)
    }
  }
}

singleResult <- function(key, lines)
{
  closedDF <- NULL
  openDF <- NULL
  
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
  
  colNames <- c("state", "name", "buy_price", "sell_price", "profit", "proffit_pp", "open", "last")
  
  result <- list()
  
  if(!is.null(closedDF))
  {
    colnames(closedDF) <- colNames
    closedDF <- closedDF[order(closedDF$proffit_pp),]
  
    result$closedDF <- closedDF
    result$totalClosed <- sprintf("%d %d %.2f", sum(closedDF$buy_price), sum(closedDF$sell_price-closedDF$buy_price), sum(closedDF$sell_price-closedDF$buy_price)/sum(closedDF$buy_price))
  }
  
  if(!is.null(openDF))
  {
    colnames(openDF) <- colNames
    openDF <- openDF[order(openDF$proffit_pp),]
    
    result$openDF <- openDF
    result$totalOpen <- sprintf("%d %d %.2f", sum(openDF$buy_price), sum(openDF$sell_price-openDF$buy_price), sum(openDF$sell_price-openDF$buy_price)/sum(openDF$buy_price))
    result$openMeanPrice <- sum(openDF$buy_price)/(nrow(openDF)*100)
  }
  
  totalDF <- rbind(openDF, closedDF)
  
  result$output <- NULL
  
  if(!is.null(totalDF$buy_price))
  {
    result$output <- sprintf("%s %d %d %.2f", key, sum(totalDF$buy_price), sum(totalDF$sell_price-totalDF$buy_price), sum(totalDF$sell_price-totalDF$buy_price)/sum(totalDF$buy_price))
  }

  result$total <- NULL
  
  if(sum(totalDF$buy_price) > 0)
  {
    result$total <- sprintf("%d %d %.2f", sum(totalDF$buy_price), sum(totalDF$sell_price-totalDF$buy_price), sum(totalDF$sell_price-totalDF$buy_price)/sum(totalDF$buy_price))
  }
  
  return(result)
}

singleResultM <- memoise(singleResult)