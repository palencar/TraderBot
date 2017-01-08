library("hashmap")
library("memoise")
library("data.table")
source("R/trade.R")
source("R/result.R")

#' @export
computeSimulation <- function(Symbols = NULL, startDate, endDate, chartDev = NULL)
{
  tradeDays <- getTradeDays(Symbols)
  tradeDays <- tradeDays[which(tradeDays >= startDate)]
  tradeDays <- tradeDays[which(tradeDays <= endDate)]

  AllSymbols <- startProbe(symbolNames = Symbols, minAge=as.integer(endDate-startDate), update=FALSE)

  forget(singleResultM)

  alertSymbols <- NULL

  smaPeriod = 400
  upperBand = 2.5
  lowerBand = -2.5
  upChange = NA
  downChange = NA
  lowLimit = NA
  stopLoss = NA
  stopGain = NA

  parameters <- data.frame(smaPeriod, upperBand, lowerBand, upChange, downChange, lowLimit, stopLoss, stopGain)

  for(symbol in AllSymbols)
  {
    map <- hashmap("1", "1")
    map$clear()

    for(tradeDate in tradeDays)
    {
      if(is.null(filterDataM(symbol, tradeDate)))
        next

      tradeDecisions <- trade(symbol, as.Date(tradeDate), parameters = parameters, map = map)

      alerts <- new.env(hash=T, parent=emptyenv())

      for(tradeDecision in tradeDecisions)
      {
        if(tradeDecision$decision != "hold")
        {
          alert <- paste(symbol, as.Date(tradeDate), tradeDecision$decision, formatC(tradeDecision$price, digits=2,format="f"), tradeDecision$reason)

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

          parStr <- paste(tradeDecision$parameters, collapse = " ")

          obj <- map[[parStr]]

          if(is.na(obj))
            map[[parStr]] <- logLine
          else
            map[[parStr]] <- paste(obj, logLine, collapse = ";", sep = ";")

          operations <- unlist(strsplit(map[[parStr]], ";"))
          lines <- strsplit(operations, " ")
          result <- singleResultM(parStr, lines, tradeDate)

          addAlerts(symbol, as.Date(tradeDate))

          if(!is.null(chartDev) && !is.null(result$output))
          {
            #lines transformar no formato correto
            #pos <- algumacoisa(lines)
            #Posit <- getOrders(symbol, pos)

            if(chartDev == "png")
            {
              path <- sprintf("charts/%s %s", symbol, parStr)
              suffix <- as.Date(tradeDate)
            }
            else
            {
              path = NULL
              suffix = NULL
            }

            smaPeriod = as.numeric(unlist(strsplit(parStr, " "))[1])
            chartSymbols(symbol, dateLimit=as.Date(tradeDate), dev=chartDev, path = path, suffix = suffix, smaPeriod = smaPeriod)
          }

          print(parStr)
          print(result)
        }
      }
    }

    forget(singleResultM)
  }

  return(alertSymbols)
}
