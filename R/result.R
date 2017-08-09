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
  closedDF <- NULL
  openDF <- NULL

  positions <- NULL
  closePosition <- FALSE

  if(nrow(lines) == 0)
    return(NULL)

  for(n in order(lines$tradeDate))
  {
    if(lines[n,"decision"] == "sell")
    {
      if(is.null(positions) == FALSE)
      {
        for(i in 1:nrow(positions))
        {
          sell_price <- as.integer(lines[n,"price"]*100)
          buy_price <- as.integer(positions[i,"price"]*100)

          newrow <- data.frame("closed", lines[n,"symbol"], buy_price, sell_price, (sell_price - buy_price), ((sell_price - buy_price) / buy_price), positions[i,"openDate"], lines[n,"tradeDate"])
          closedDF <- rbind(closedDF, newrow)
        }
        positions <- NULL
        openDate <- NULL
        closePosition <- TRUE
      }
    }

    if(lines[n,"decision"] == "buy")
    {
      openDate <- lines[n,"tradeDate"]
      names(openDate) <- c("openDate")
      price <- lines[n,"price"]

      positions <- rbind.data.frame(positions, data.frame(openDate, price))
    }
  }

  if(closePosition == FALSE)
  {
    if(is.null(lastDay))
    {
      lastDay <- last(index(base::get(as.character(unique(lines[,"symbol"])))))
    }

    if(is.null(positions) == FALSE)
    {
      for(i in 1:nrow(positions))
      {
        sell_price <- as.numeric(Cl(tail(base::get(as.character(unique(lines[i,"symbol"]))), 1)) * 100)
        buy_price <- as.integer(positions[i,"price"]*100)

        newrow <- data.frame("open", as.character(unique(lines[i,"symbol"])), buy_price, sell_price, (sell_price - buy_price), ((sell_price - buy_price) / buy_price), positions[i,"openDate"], lastDay)
        openDF <- rbind(openDF, newrow)
      }
    }
    positions <- NULL
  }
  closePosition <- FALSE

  colNames <- c("state", "name", "buy_price", "sell_price", "profit", "proffit_pp", "open", "last")

  result <- list()

  if(!is.null(closedDF))
  {
    colnames(closedDF) <- colNames
    closedDF <- closedDF[order(closedDF$proffit_pp),]

    result$closedDF <- closedDF
    buy     <- sum(closedDF$buy_price)
    gain    <- sum(closedDF$sell_price-closedDF$buy_price)
    proffit <- sum(closedDF$sell_price-closedDF$buy_price)/sum(closedDF$buy_price)

    result$totalClosed <- data.frame(buy, gain, proffit)
    result$closedMeanPrice <- sum(closedDF$buy_price)/(nrow(closedDF)*100)
  }

  if(!is.null(openDF))
  {
    colnames(openDF) <- colNames
    openDF <- openDF[order(openDF$proffit_pp),]

    result$openDF <- openDF
    buy     <- sum(openDF$buy_price)
    gain    <- sum(openDF$sell_price-openDF$buy_price)
    proffit <- sum(openDF$sell_price-openDF$buy_price)/sum(openDF$buy_price)

    result$totalOpen <- data.frame(buy, gain, proffit)
    result$openMeanPrice <- sum(openDF$buy_price)/(nrow(openDF)*100)
  }

  totalDF <- rbind(openDF, closedDF)

  result$output <- NULL

  if(!is.null(totalDF$buy_price))
  {
    buy     <- sum(totalDF$buy_price)
    gain    <- sum(totalDF$sell_price-totalDF$buy_price)
    proffit <- sum(totalDF$sell_price-totalDF$buy_price)/sum(totalDF$buy_price)
    result$output <- data.frame(buy, gain, proffit)
  }

  result$total <- NULL

  if(sum(totalDF$buy_price) > 0)
  {
    capital <- sum(totalDF$buy_price)
    gain    <- sum(totalDF$sell_price-totalDF$buy_price)
    proffit <- sum(totalDF$sell_price-totalDF$buy_price)/sum(totalDF$buy_price)
    result$total <- data.frame(capital, gain, proffit)
  }

  return(result)
}

singleResultM <- memoise(singleResult)
