library("memoise")


fBullBear <-function(seq, period, context = 500)
{
  sma <- tail(SMA(seq, period), context + 1)

  rl <- rle(sign(diff(as.vector(sma))))
  len <- sum(as.vector(na.exclude(rl$lengths)))

  bull  <- sum(as.vector(na.exclude(rl$lengths[rl$values == 1])))/len
  bear  <- sum(as.vector(na.exclude(rl$lengths[rl$values == -1])))/len

  retValue <- c()
  retValue$bull <- bull
  retValue$bear <- bear

  return(retValue)
}

fPreventMinMax <- function(symbol, period)
{
  cantBuy <- NULL
  cantSell <- NULL

  obj <- base::get(symbol)[period]
  hiObj <- Hi(obj)
  loObj <- Lo(obj)
  tradeDate <- index(xts::last(obj))

  if(is.null(cantBuy) && all(tail(loObj[paste0("/", index(first(tail(obj, 6))))] > min(tail(loObj, 5)), 50)))
  {
    cantBuy <- sprintf("DO NOT BUY: %s | [%s] min short term", symbol, tradeDate, index(obj[which.min(tail(loObj, 50))]))
  }

  if(is.null(cantBuy) && all(tail(loObj[paste0("/", index(first(tail(obj, 11))))] > min(tail(loObj, 10)), 200)))
  {
    cantBuy <- sprintf("DO NOT BUY: %s | [%s] min mid term", symbol, tradeDate, index(obj[which.min(tail(loObj, 200))]))
  }

  if(is.null(cantBuy) && all(tail(loObj[paste0("/", index(first(tail(obj, 31))))] > min(tail(loObj, 30)), 600)))
  {
    cantBuy <- sprintf("DO NOT BUY: %s | [%s] min long term", symbol, tradeDate, index(obj[which.min(tail(loObj, 600))]))
  }

  #if(is.null(cantBuy) && min(tail(loObj, 300)) * 1.02 > as.numeric(loObj[tradeDate]))
  #{
  #  cantBuy <- sprintf("DO NOT BUY: %s | [%s] [%s] near minimal [%s]", symbol, tradeDate, as.numeric(loObj[tradeDate]), min(tail(loObj, 300)))
  #}

  if(is.null(cantSell) && all(tail(hiObj[paste0("/", index(first(tail(obj, 6))))] < max(tail(hiObj, 5)), 50)))
  {
    cantSell <- sprintf("DO NOT SELL: %s | [%s] max short term", symbol, tradeDate, index(obj[which.max(tail(hiObj, 50))]))
  }

  if(is.null(cantSell) && all(tail(hiObj[paste0("/", index(first(tail(obj, 11))))] < max(tail(hiObj, 10)), 200)))
  {
    cantSell <- sprintf("DO NOT SELL: %s | [%s] max mid term", symbol, tradeDate, index(obj[which.max(tail(hiObj, 200))]))
  }

  if(is.null(cantSell) && all(tail(hiObj[paste0("/", index(first(tail(obj, 31))))] < max(tail(hiObj, 30)), 600)))
  {
    cantSell <- sprintf("DO NOT SELL: %s | [%s] max long term", symbol, tradeDate, index(obj[which.max(tail(hiObj, 600))]))
  }

  #if(is.null(cantSell) && max(tail(hiObj, 300)) * 0.98 < as.numeric(hiObj[tradeDate]))
  #{
  #  cantSell <- sprintf("DO NOT SELL: %s | [%s] [%s] near maximal [%s]", symbol, tradeDate, as.numeric(hiObj[tradeDate]), max(tail(hiObj, 300)))
  #}

  retValue <- c()
  retValue$cantBuy <- cantBuy
  retValue$cantSell <- cantSell

  return(retValue)
}

trade <- function(symbol, tradeDate, parameters = NULL, profit = NULL, type = "none", verbose = FALSE)
{
  cantBuy <- NULL
  cantSell <- NULL

  if(is.null(parameters))
    return(NULL)

  obj <- base::get(symbol)[sprintf("/%s", tradeDate)]
  seq <- xts(as.double((Hi(obj)+Lo(obj)+Cl(obj))/3), index(obj))

  d <- diff(tail(obj, 200)) == 0
  d[1,] <- FALSE
  rp <- mean(as.numeric(d))
  if(rp > 0.15)
  {
    if(is.null(profit))
      return(NULL)

    cantBuy <- paste0("DO NOT BUY: ", symbol, " | [", tradeDate, "] Repeated prices : ", rp, " > 0.15")
    cantSell <- paste0("DO NOT SELL: ", symbol, " | [", tradeDate, "] Repeated prices : ", rp, " > 0.15")
  }

  if(length(seq) < parameters$smaPeriod + 500)
    return(NULL)

  period <- tail(index(obj), 500)
  period <- paste0(first(period), "::", last(period))

  sma <- SMA(seq, parameters$smaPeriod)
  dif <- as.double(na.omit(tail(seq-sma, 500)))
  sddf <- sd(dif)
  sdp <- (last(seq)-last(sma))/sddf

  sdsma <- round(sddf/last(sma), 3)
  if(is.null(cantBuy) && is.null(cantSell) && sdsma < 0.05)
  {
    cantBuy <- paste0("DO NOT BUY: ", symbol, " | [", tradeDate, "] sd/sma : ", sdsma, " < 0.05")
    cantSell <- paste0("DO NOT SELL: ", symbol, " | [", tradeDate, "] sd/sma : ", sdsma, " < 0.05")

    if(is.null(profit))
      return(NULL)
  }

  if(is.na(sdp))
  {
    warning(paste0("sdp: NA ", symbol))
    return(NULL)
  }

  lastValue <- as.numeric(Cl(xts::last(obj)))
  lastHi <- as.numeric(Hi(xts::last(obj)))
  lastLo <- as.numeric(Lo(xts::last(obj)))

  objPeriod <- obj[period]
  maxValue <- as.numeric(Hi(objPeriod[which.max(Hi(objPeriod))]))
  minValue <- as.numeric(Lo(objPeriod[which.min(Lo(objPeriod))]))

  maxChange <- (lastValue-maxValue)/maxValue
  minChange <- (lastValue-minValue)/minValue

  pMinMax <- fPreventMinMax(symbol, period)

  cantBuy  <- unique(c(cantBuy, pMinMax$cantBuy))
  cantSell <- unique(c(cantSell, pMinMax$cantSell))

  high <- Hi(obj)
  maxValue <- as.numeric(high[which.max(high)])
  maxDate <- index(high[which.max(high)])

  lowAfter <- Lo(obj)[sprintf("%s/", maxDate)]
  minAfter <- as.numeric(lowAfter[which.min(lowAfter)])

  if(is.null(cantBuy) && !is.na(parameters$lowLimit) && (minAfter / maxValue) < parameters$lowLimit)
  {
    cantBuy <- sprintf("DO NOT BUY: %s | [%s] Min [%f] After / Max [%s][%f] : [%f]", symbol, period, minAfter, maxDate, maxValue, (minAfter / maxValue))
  }

  if(is.null(cantBuy) && !is.na(parameters$lowLimit) && (lastLo / maxValue) < parameters$lowLimit)
  {
    cantBuy <- sprintf("DO NOT BUY: %s | [%s] Last [%f] / Max [%s][%f] : [%f]", symbol, period, lastLo, maxDate, maxValue, (lastLo / maxValue))
  }

  mBullBear <-fBullBear(seq, parameters$smaPeriod)

  bull  <- mBullBear$bull
  bear  <- mBullBear$bear

  decision <- "hold"
  reason <- NULL

  if(!is.na(parameters$lowerBand))
  {
    lower <- parameters$lowerBand + (as.numeric(maxChange))
  }

  if(!is.na(parameters$upperBand))
  {
    upper <- parameters$upperBand + (as.numeric(minChange))
  }

  if(is.null(cantBuy) && !is.na(parameters$bullBuy) && as.numeric(bull) < parameters$bullBuy)
  {
    cantBuy <- sprintf("DO NOT BUY: %s | [%s] Bullish [%.2f] < [%.2f]", symbol, period, bull, parameters$bullBuy)
  }

  if(is.null(cantSell) && !is.na(parameters$bullSell) && as.numeric(bull) > parameters$bullSell)
  {
    cantSell <- sprintf("DO NOT SELL: %s | [%s] Bullish [%.2f] > [%.2f]", symbol, period, bull, parameters$bullSell)
  }

  if(is.null(cantSell) && !is.na(parameters$bearSell) && as.numeric(bear) < parameters$bearSell)
  {
    cantSell <- sprintf("DO NOT SELL: %s | [%s] Bearish [%.2f] < [%.2f]", symbol, period, bear, parameters$bearSell)
  }

  if(is.null(cantBuy) && !is.na(parameters$bearBuy) && as.numeric(bear) > parameters$bearBuy)
  {
    cantBuy <- sprintf("DO NOT BUY: %s | [%s] Bearish [%.2f] > [%.2f]", symbol, period, bear, parameters$bearBuy)
  }

  if(is.null(cantBuy) && !is.na(parameters$downChange) && as.numeric(maxChange) < parameters$downChange)
  {
    cantBuy <- sprintf("DO NOT BUY: %s | [%s] Max Change : [%f]", symbol, period, maxChange)
  }

  if(is.null(cantSell) && !is.na(parameters$upChange) && as.numeric(minChange) > parameters$upChange)
  {
    cantSell <- sprintf("DO NOT SELL: %s | [%s] Min Change : [%f]", symbol, period, minChange)
  }

  isStop <- FALSE

  if(is.null(lower) || is.na(lower))
  {
    warning(paste("lower:", lower))
  }
  else if(sdp < lower)
  {
    if(is.null(cantBuy))
    {
      decision <- "buy"
      reason <- sprintf("sdp < %1.1f -> buy", lower)
    }
    if(type == "short")
    {
      decision <- "buy"
      isStop <- TRUE
      reason <- sprintf("sdp < %1.1f [stop] -> buy", lower)
    }
  }

  if(is.null(upper) || is.na(upper))
  {
    warning(paste("upper:", upper))
  }
  else if(sdp > upper)
  {
    if(is.null(cantSell))
    {
      decision <- "sell"
      reason <- sprintf("sdp > %1.1f -> sell", upper)
    }
    if(type == "long")
    {
      decision <- "sell"
      isStop <- TRUE
      reason <- sprintf("sdp > %1.1f [stop] -> sell", upper)
    }
  }

  if(!is.null(profit))
  {
    if(type == "long" && !is.na(parameters$stopGain) && (1+profit) >= parameters$stopGain) #Stop gain
    {
      isStop <- TRUE
      decision <- "sell"
      reason <- sprintf("Stop Gain [%.2f %.2f] -> sell", parameters$stopGain, profit)
    }

    if(type == "short" && !is.na(parameters$stopLoss) && (1+profit) <= parameters$stopLoss) #Stop loss
    {
      isStop <- TRUE
      decision <- "buy"
      reason <- sprintf("Stop Loss [%.2f %.2f] -> buy", parameters$stopLoss, profit)
    }

    if(type == "short" && !is.na(parameters$stopGain) && (1+profit) >= parameters$stopGain) #Stop gain
    {
      isStop <- TRUE
      decision <- "buy"
      reason <- sprintf("Stop Gain [%.2f %.2f] -> buy", parameters$stopGain, profit)
    }

    if(type == "long" && !is.na(parameters$stopLoss) && (1+profit) <= parameters$stopLoss) #Stop loss
    {
      isStop <- TRUE
      decision <- "sell"
      reason <- sprintf("Stop Loss [%.2f %.2f] -> sell", parameters$stopLoss, profit)
    }
  }

  if(verbose && !is.null(cantSell))
  {
    print(cantSell)
  }

  if(verbose && !is.null(cantBuy))
  {
    print(cantBuy)
  }

  tradeDecision <- c()

  tradeDecision$decision <- decision
  tradeDecision$stop <- isStop
  tradeDecision$reason <- reason
  tradeDecision$price <- lastValue

  return(tradeDecision)
}
