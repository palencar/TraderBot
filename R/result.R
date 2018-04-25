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

singleResult <- function(lines, lastDay = NULL)
{
  closedDF <- list()
  openDF <- list()

  positions <- NULL
  closePosition <- FALSE

  if(nrow(lines) == 0)
    return(NULL)

  symbolName <- as.character(lines$symbol[1])

  op = rbind(xts(lines$price, order.by = lines$tradeDate), Cl(base::get(symbolName)[index(rbind(getSplits.db(symbolName), getDividends.db(symbolName)))]))
  lines$price <- adjustOperations(symbolName, op[!duplicated(index(op)), ])[lines$tradeDate]

  for(n in order(lines$tradeDate))
  {
    if(lines$decision[n] == "sell")
    {
      if(is.null(positions) == FALSE)
      {
        for(i in 1:nrow(positions))
        {
          sell_price <- as.integer(lines$price[n]*100)
          buy_price <- as.integer(positions$price[i]*100)
          len <- length(closedDF)
          closedDF[[len+1]] <- data.frame("closed", symbolName, buy_price, sell_price, (sell_price - buy_price), ((sell_price - buy_price) / buy_price), positions$openDate[i], lines$tradeDate[n])
        }
        positions <- NULL
        openDate <- NULL
        closePosition <- TRUE
      }
    }

    if(lines$decision[n] == "buy")
    {
      openDate <- lines$tradeDate[n]
      names(openDate) <- c("openDate")
      price <- as.numeric(lines$price[n])

      positions <- rbind.data.frame(positions, data.table(openDate, price))
    }
  }

  if(is.null(positions) == FALSE)
  {
    if(is.null(lastDay))
    {
      lastDay <- last(index(base::get(symbolName)))
    }

    for(i in 1:nrow(positions))
    {
      if(positions$openDate[i] == lastDay)
        next
      sell_price <- as.numeric(Cl(tail(base::get(symbolName), 1)) * 100)
      buy_price <- as.integer(positions$price[i]*100)
      len <- length(openDF)
      openDF[[len+1]] <- data.frame("open", symbolName, buy_price, sell_price, (sell_price - buy_price), ((sell_price - buy_price) / buy_price), positions$openDate[i], lastDay)
    }

    positions <- NULL
  }

  closedDF <- rbindlist(closedDF)
  openDF <- rbindlist(openDF)

  closePosition <- FALSE

  colNames <- c("state", "name", "buy_price", "sell_price", "profit", "profit_pp", "open", "last")

  result <- list()

  if(nrow(closedDF) > 0)
  {
    colnames(closedDF) <- colNames
    closedDF <- closedDF[order(closedDF$profit_pp),]

    result$closedDF <- closedDF
    buy     <- sum(closedDF$buy_price)
    gain    <- sum(closedDF$sell_price-closedDF$buy_price)
    profit  <- sum(closedDF$sell_price-closedDF$buy_price)/sum(closedDF$buy_price)

    result$totalClosed <- data.frame(buy, gain, profit)
    result$closedMeanPrice <- sum(closedDF$buy_price)/(nrow(closedDF)*100)
  }

  if(nrow(openDF) > 0)
  {
    colnames(openDF) <- colNames
    openDF <- openDF[order(openDF$profit_pp),]

    result$openDF <- openDF
    buy     <- sum(openDF$buy_price)
    gain    <- sum(openDF$sell_price-openDF$buy_price)
    profit  <- sum(openDF$sell_price-openDF$buy_price)/sum(openDF$buy_price)

    result$totalOpen <- data.frame(buy, gain, profit)
    result$openMeanPrice <- sum(openDF$buy_price)/(nrow(openDF)*100)
  }

  totalDF <- rbind(openDF, closedDF)

  result$output <- NULL

  if(!is.null(totalDF$buy_price))
  {
    buy     <- sum(totalDF$buy_price)
    gain    <- sum(totalDF$sell_price-totalDF$buy_price)
    profit  <- sum(totalDF$sell_price-totalDF$buy_price)/sum(totalDF$buy_price)
    result$output <- data.frame(buy, gain, profit)
  }

  result$total <- NULL

  if(sum(totalDF$buy_price) > 0)
  {
    capital <- sum(totalDF$buy_price)
    gain    <- sum(totalDF$sell_price-totalDF$buy_price)
    profit  <- sum(totalDF$sell_price-totalDF$buy_price)/sum(totalDF$buy_price)
    result$total <- data.frame(capital, gain, profit)
  }

  return(result)
}

singleResultM <- memoise(singleResult)
