source("trade.R")
source("result.R")

showAll <- FALSE
report <- TRUE

singleResult <- function(symbol, key, lines)
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
      pars <- gsub("_", " ", key)
      
      strOut <- sprintf("%s %d %d %.2f", pars, sum(totalDF$buy_price), sum(totalDF$sell_price-totalDF$buy_price), sum(totalDF$sell_price-totalDF$buy_price)/sum(totalDF$buy_price))
      cat(file = sprintf("result/%s.txt", symbol), strOut, sep = "\n", append = TRUE)
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

computeBacktest <- function(Symbols, startDate, endDate, printCharts = FALSE)
{
  tradeDays <- getTradeDays()
  
  AllSymbols <- startProbe(symbolNames = Symbols, minAge=200, update=FALSE)
  
  alertSymbols <- NULL
  
  charts <- new.env(hash=T, parent=emptyenv())
  
  for(symbol in AllSymbols)
  {
    #if(file.exists(sprintf("result/%s.rds", symbol)))
    #{
    #  results <- readRDS(sprintf("result/%s.rds", symbol))
    #}
    #else
    {
      results <- new.env(hash=T, parent=emptyenv())
    }
    
    for(tradeDate in seq.Date(as.Date(startDate), as.Date(endDate), by="+1 days"))
    {
      if((as.Date(tradeDate) %in% as.Date(tradeDays)) == FALSE || length(as.Date(tradeDate)) == 0 || is.null(filterData(symbol, tradeDate)))
        next
      
      smaPeriod = sample(50:300, 5)
      upperBand = as.numeric(formatC(runif(4, min=0, max=2), digits=2,format="f"))
      lowerBand = as.numeric(formatC(runif(4, min=-2, max=-1), digits=2,format="f"))
      upChange = as.numeric(formatC(runif(4, min=0, max=1), digits=2,format="f"))
      downChange = as.numeric(formatC(runif(4, min=-1, max=0), digits=2,format="f"))
      lowLimit = as.numeric(formatC(runif(4, min=0, max=1), digits=2,format="f"))
      stopLoss = as.numeric(formatC(runif(4, min=0.5, max=1), digits=2,format="f"))
      stopGain = as.numeric(formatC(runif(4, min=1, max=2), digits=2,format="f"))
      
      price <- simPrice(symbol, tradeDate)
      
      tradeDecisions <- trade(symbol, as.Date(tradeDate), smaPeriod = smaPeriod, upperBand = upperBand, lowerBand = lowerBand, upChange = upChange, downChange = downChange, lowLimit = lowLimit, stopLoss = stopLoss, stopGain = stopGain, price = price)
      
      alerts <- new.env(hash=T, parent=emptyenv())
      
      for(tradeDecision in tradeDecisions)
      {
        if(tradeDecision$decision != "hold")
        {
          alert <- paste(symbol, as.Date(tradeDate), tradeDecision$decision, tradeDecision$reason)
          
          if(is.null(alerts[[alert]]))
          {
            print(alert)
            alerts[[alert]] <- TRUE
          }
          
          if(symbol %in% alertSymbols == FALSE)
          {
            alertSymbols <- c(alertSymbols, symbol)
          }
          
          price <- sprintf("%.2f", tradeDecision$price)
          logLine <- paste(symbol, as.Date(tradeDate), tradeDecision$decision, price, collapse = " ")
          
          parStr <- sprintf("%03d %1.2f %1.2f %1.2f %1.2f %1.2f %1.2f %1.2f", tradeDecision$parameters[1], tradeDecision$parameters[2], tradeDecision$parameters[3],
                            tradeDecision$parameters[4], tradeDecision$parameters[5], tradeDecision$parameters[6], tradeDecision$parameters[7], tradeDecision$parameters[8], tradeDecision$parameters[9])

          results[[parStr]] <- c(results[[parStr]], logLine)
          
          suffix <- sprintf("sma%03d", tradeDecision$parameters[1])
          
          key <- paste(symbol, tradeDate, suffix)
          
          if(printCharts && is.null(charts[[key]]))
          {
            chartSymbols(symbol, dateLimit=as.Date(tradeDate), dev="png", suffix = suffix, smaPeriod = tradeDecision$parameters[1])
            charts[[key]] <- TRUE
          }
        }
      }
    }
    
    for(key in ls(results))
    {
      singleResult(symbol, key, results[[key]])
    }
  }
  
  return(alertSymbols)
}
