library("memoise")
source("R/adjust.R")

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

openResult <- function(operations, symbol, lastTime)
{
  ops <- xts(operations$price, order.by = operations$tradeDate)
  lastPr <- Cl(base::get(symbol)[lastTime])

  adjustOp <- head(adjustOperations(symbolName = symbol, rbind(ops, lastPr)), -1)

  profit <- NULL

  if(unique(operations$decision) == "buy")
    profit <- sum(as.numeric(lastPr)-adjustOp)/sum(adjustOp)
  if(unique(operations$decision) == "sell")
    profit <- sum(adjustOp-as.numeric(lastPr))/sum(adjustOp)

  return(profit)
}

singleResult <- function(lines, lastDay = NULL)
{
  if(nrow(lines) == 0)
    return(NULL)

  closedDF <- data.table()
  openDF <- data.table()
  positions <- list()

  position <- "none"

  symbolName <- unique(lines$symbol)
  clObj <- Cl(base::get(symbolName))
  symbol <- unlist(strsplit(symbolName, "[.]"))[1]

  if(is.null(lastDay))
    lastDay <- last(index(clObj))

  clObj <- clObj[paste0("/", lastDay)]
  op = rbind(xts(lines$price, order.by = lines$tradeDate), last(clObj[as.character(index(rbind(getSplits.db(symbol), getDividends.db(symbol))))]))
  lines$price <- adjustOperations(symbolName, op[!duplicated(index(op)), ])[lines$tradeDate]

  for(n in order(lines$tradeDate))
  {
    if(position == "none" || (position == "long" && lines$decision[n] == "buy") || (position == "short" && lines$decision[n] == "sell"))
    {
      positions[[length(positions)+1]] <- data.table(openDate=lines$tradeDate[n],
                                                     type=ifelse(lines[n]$decision == "buy", "long", "short"),
                                                     price=as.numeric(lines$price[n]))

      position <- ifelse(lines[n]$decision == "buy", "long", "short")

      next
    }

    if(position == "long" && lines$decision[n] == "sell")
    {
      pos <- rbindlist(positions)

      buy_price <- pos$price
      sell_price <- as.numeric(lines$price[n])

      closedDF <- rbind(closedDF, data.table(state = "closed", type = position, name = symbolName, buy_price, sell_price, profit = (sell_price - buy_price),
                                             profit_pp = ((sell_price - buy_price) / buy_price), open = pos$openDate, last = lines$tradeDate[n]))

      positions <- list()

      if(lines[n,stop] == FALSE)
        positions[[length(positions)+1]] <- data.table(openDate=lines$tradeDate[n],
                                                       type=ifelse(lines[n]$decision == "buy", "long", "short"),
                                                       price=as.numeric(lines$price[n]))

      position <- ifelse(lines[n,stop], "none", "short")

      next
    }

    if(position == "short" && lines$decision[n] == "buy")
    {
      pos <- rbindlist(positions)

      buy_price <- as.numeric(lines$price[n])
      sell_price <- pos$price

      closedDF <- rbind(closedDF, data.table(state = "closed", type = position, name = symbolName, buy_price, sell_price, profit = (sell_price - buy_price),
                                             profit_pp = ((sell_price - buy_price) / sell_price), open = pos$openDate, last = lines$tradeDate[n]))

      positions <- list()

      if(lines[n,stop] == FALSE)
        positions[[length(positions)+1]] <- data.table(openDate=lines$tradeDate[n],
                                                       type=ifelse(lines[n]$decision == "buy", "long", "short"),
                                                       price=as.numeric(lines$price[n]))

      position <- ifelse(lines[n,stop], "none", "long")

      next
    }
  }

  pos <- rbindlist(positions)

  if(position == "long")
  {
    buy_price <- pos$price
    sell_price <- as.numeric(tail(clObj, 1))

    openDF <- data.table(state = "open", type = position, name = symbolName, buy_price, sell_price, profit = (sell_price - buy_price),
                         profit_pp = ((sell_price - buy_price) / buy_price), open = pos$openDate, last = lastDay)
  }

  if(position == "short")
  {
    buy_price <- as.numeric(tail(clObj, 1))
    sell_price <- pos$price

    openDF <- data.table(state = "open", type = position, name = symbolName, buy_price, sell_price, profit = (sell_price - buy_price),
                         profit_pp = ((sell_price - buy_price) / sell_price), open = pos$openDate, last = lastDay)
  }

  maxDrawdown <- function(open, last, type)
  {
    if(type == "long")
      return(max(1-clObj[paste0(open, "/", last)]/cummax(clObj[paste0(open, "/", last)])))
    if(type == "short")
      return(max(clObj[paste0(open, "/", last)]/cummin(clObj[paste0(open, "/", last)])-1))
  }

  result <- list()

  if(nrow(closedDF) > 0)
  {
    closedDF[, maxDrawdown := mapply(maxDrawdown, open, last, type)]
    closedDF <- closedDF[order(closedDF$profit_pp, decreasing = TRUE),]

    result$closedDF <- closedDF

    result$closedMeanProfit <- sum(closedDF$sell_price-closedDF$buy_price) /
                                (sum(closedDF$buy_price[closedDF$type == "long"]) + sum(closedDF$sell_price[closedDF$type == "short"]))
  }

  if(nrow(openDF) > 0)
  {
    openDF[, maxDrawdown := mapply(maxDrawdown, open, last, type)]
    openDF <- openDF[order(openDF$profit_pp, decreasing = TRUE),]

    result$openDF <- openDF

    result$openMeanProfit <- sum(openDF$sell_price-openDF$buy_price) /
                              (sum(openDF$buy_price[openDF$type == "long"]) + sum(openDF$sell_price[openDF$type == "short"]))
  }

  return(result)
}
