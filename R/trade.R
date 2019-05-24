library("memoise")


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
  if(type == "none" && rp > 0.15)
  {
    cantBuy <- paste0(symbol, " | [", tradeDate, "] Repeated prices : ", rp, " > 0.15")
    cantSell <- paste0(symbol, " | [", tradeDate, "] Repeated prices : ", rp, " > 0.15")

    if(is.null(profit))
      return(NULL)
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
  if(type == "none" && is.null(cantBuy) && is.null(cantSell) && sdsma < 0.05)
  {
    cantBuy <- paste0(symbol, " | [", tradeDate, "] sd/sma : ", sdsma, " < 0.05")
    cantSell <- paste0(symbol, " | [", tradeDate, "] sd/sma : ", sdsma, " < 0.05")

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

  high <- Hi(objPeriod)
  low <- Lo(objPeriod)

  maxV <- as.numeric(high[which.max(high)])
  maxDate <- index(high[which.max(high)])

  lowAfter <- low[sprintf("%s/", maxDate)]
  minAfter <- as.numeric(lowAfter[which.min(lowAfter)])

  minV <- as.numeric(low[which.min(low)])
  minDate <- index(low[which.min(low)])

  highAfter <- low[sprintf("%s/", maxDate)]
  maxAfter <- as.numeric(highAfter[which.max(highAfter)])

  if(type != "short" && is.null(cantBuy) && !is.na(parameters$lowLimit) && (minAfter / maxV) < parameters$lowLimit)
    cantBuy <- sprintf("%s | [%s] Min [%f] After / Max [%s][%f] : [%f]", symbol, period, minAfter, maxDate, maxV, (minAfter / maxV))

  if(type != "short" && is.null(cantBuy) && !is.na(parameters$lowLimit) && (lastLo / maxV) < parameters$lowLimit)
    cantBuy <- sprintf("%s | [%s] Last [%f] / Max [%s][%f] : [%f]", symbol, period, lastLo, maxDate, maxV, (lastLo / maxV))

  if(type != "short" && is.null(cantSell) && !is.na(parameters$highLimit) && (maxAfter / minV) > parameters$highLimit)
    cantSell <- sprintf("%s | [%s] Max [%f] After / Min [%s][%f] : [%f]", symbol, period, maxAfter, minDate, minV, (maxAfter / minV))

  if(type != "short" && is.null(cantSell) && !is.na(parameters$highLimit) && (lastHi / minV) > parameters$highLimit)
    cantSell <- sprintf("%s | [%s] Last [%f] / Min [%s][%f] : [%f]", symbol, period, lastHi, minDate, minV, (lastHi / minV))

  decision <- "hold"
  reason <- NULL

  if(!is.na(parameters$lowerBand))
    lower <- parameters$lowerBand + (as.numeric(maxChange))

  if(!is.na(parameters$upperBand))
    upper <- parameters$upperBand + (as.numeric(minChange))

  if(is.null(cantBuy) && all(tail(low[paste0("/", index(first(tail(obj, 6))))], 50) > min(tail(low, 5)) * 0.99))
    cantBuy <- "short"

  if(is.null(cantBuy) && all(tail(low[paste0("/", index(first(tail(obj, 11))))], 100) > min(tail(low, 10)) * 0.99))
    cantBuy <- "mid"

  if(is.null(cantBuy) && all(tail(low[paste0("/", index(first(tail(obj, 21))))], 200) > min(tail(low, 20)) * 0.99))
    cantBuy <- "long"

  if(is.null(cantBuy) && all(tail(low[paste0("/", index(first(tail(obj, 31))))], 300) > min(tail(low, 30)) * 0.99))
    cantBuy <- "very long"

  if(is.null(cantBuy) && which.min(tail(low, 500))/500 >= 0.9)
    cantBuy <- "very long"

  if(is.null(cantSell) && all(tail(high[paste0("/", index(first(tail(obj, 6))))], 50) < max(tail(high, 5)) * 1.01))
    cantSell <- "short"

  if(is.null(cantSell) && all(tail(high[paste0("/", index(first(tail(obj, 11))))], 100) < max(tail(high, 10)) * 1.01))
    cantSell <- "mid"

  if(is.null(cantSell) && all(tail(high[paste0("/", index(first(tail(obj, 21))))], 200) < max(tail(high, 20)) * 1.01))
    cantSell <- "long"

  if(is.null(cantSell) && all(tail(high[paste0("/", index(first(tail(obj, 31))))], 300) < max(tail(high, 30)) * 1.01))
    cantSell <- "very long"

  if(is.null(cantSell) && which.max(tail(high, 500))/500 >= 0.9)
    cantSell <- "very long"

  isStop <- FALSE

  if(is.null(lower) || is.na(lower))
  {
    warning(paste("lower:", lower))
  }
  else if(sdp < lower)
  {
    if(type == "none" && is.null(cantBuy))
    {
      decision <- "buy"
      reason <- sprintf("sdp < %1.1f -> buy", lower)
    }
    if(type == "short" && (is.null(cantBuy) || cantBuy == "long" || cantBuy == "very long"))
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
    if(type == "none" && is.null(cantSell))
    {
      decision <- "sell"
      reason <- sprintf("sdp > %1.1f -> sell", upper)
    }
    if(type == "long" && (is.null(cantSell) || cantSell == "long" || cantSell == "very long"))
    {
      decision <- "sell"
      isStop <- TRUE
      reason <- sprintf("sdp > %1.1f [stop] -> sell", upper)
    }
  }

  if(!is.null(profit))
  {
    if(type == "long" && !is.na(parameters$stopGainLong) && (1+profit) >= parameters$stopGainLong) #Stop gain
    {
      isStop <- TRUE
      decision <- "sell"
      reason <- sprintf("Stop Gain [%.2f %.2f] -> sell", parameters$stopGainLong, profit)
    }

    if(type == "short" && !is.na(parameters$stopLoss) && (1+profit) <= parameters$stopLoss) #Stop loss
    {
      isStop <- TRUE
      decision <- "buy"
      reason <- sprintf("Stop Loss [%.2f %.2f] -> buy", parameters$stopLoss, profit)
    }

    if(type == "short" && !is.na(parameters$stopGainShort) && (1+profit) >= parameters$stopGainShort) #Stop gain
    {
      isStop <- TRUE
      decision <- "buy"
      reason <- sprintf("Stop Gain [%.2f %.2f] -> buy", parameters$stopGainShort, profit)
    }

    if(type == "long" && !is.na(parameters$stopLoss) && (1+profit) <= parameters$stopLoss) #Stop loss
    {
      isStop <- TRUE
      decision <- "sell"
      reason <- sprintf("Stop Loss [%.2f %.2f] -> sell", parameters$stopLoss, profit)
    }
  }

  if(verbose && !is.null(cantSell))
    print(paste0("DO NOT SELL : ", cantSell))

  if(verbose && !is.null(cantBuy))
    print(paste0("DO NOT BUY : ", cantBuy))

  tradeDecision <- c()

  tradeDecision$decision <- decision
  tradeDecision$stop <- isStop
  tradeDecision$reason <- reason
  tradeDecision$price <- lastValue

  return(tradeDecision)
}
