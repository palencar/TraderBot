library("memoise")


fMaxChange <- function(symbol, period, lastValue)
{
  objPeriod <- base::get(symbol)[period]

  high <- Hi(objPeriod)

  maxValue <- as.numeric(high[which.max(high)])
  maxDate <- index(high[which.max(high)])

  obj <- objPeriod[sprintf("%s/", maxDate)]
  obj <- xts((Hi(obj)+Lo(obj)+Cl(obj))/3)

  lr <- linearRegression(obj)
  maxChange <- as.numeric((lr$coef*365)/lastValue)
  if(is.na(maxChange) || nrow(obj) < 30)
  {
    maxChange <- 0
  }
  return(maxChange)
}

fMinChange <- function(symbol, period, lastValue)
{
  objPeriod <- base::get(symbol)[period]

  low <- Lo(objPeriod)

  minValue <- as.numeric(low[which.min(low)])
  minDate <- index(low[which.min(low)])

  obj <- objPeriod[sprintf("%s/", minDate)]
  obj <- xts((Hi(obj)+Lo(obj)+Cl(obj))/3)

  lr <- linearRegression(obj)
  minChange <- as.numeric((lr$coef*365)/lastValue)
  if(is.na(minChange) || nrow(obj) < 30)
  {
    minChange <- 0
  }
  return(minChange)
}

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
  tradeDate <- index(xts::last(obj))

  lowYear <- tail(Lo(obj), 250) #rougthly 1 year (on daily timeframe)
  minYear <- index(lowYear[which.min(lowYear)])
  if(nrow(obj[paste0(minYear, "::", tradeDate)]) <= 7)
  {
    str <- sprintf("DO NOT BUY: %s | [%s] Min Year [%s]", symbol, tradeDate, minYear)
    cantBuy <- c(cantBuy[cantBuy != str], str)
  }

  if(length(Lo(obj)[tradeDate]) == 1 &&
     (as.numeric(lowYear[which.min(lowYear)]) * 1.05) > as.numeric(Lo(obj)[tradeDate]))
  {
    str <- sprintf("DO NOT BUY: %s | [%s] [%s] near minimal [%s]", symbol, tradeDate, as.numeric(Lo(obj)[tradeDate]), as.numeric(lowYear[which.min(lowYear)]))
    cantBuy <- c(cantBuy[cantBuy != str], str)
  }

  low2Year <- tail(Lo(obj), 500)
  min2Year <- index(low2Year[which.min(low2Year)])
  if(nrow(obj[paste0(min2Year, "::", tradeDate)]) <= 14)
  {
    str <- sprintf("DO NOT BUY: %s | [%s] Min 2 Year [%s]", symbol, tradeDate, min2Year)
    cantBuy <- c(cantBuy[cantBuy != str], str)
  }

  highYear <- tail(Hi(obj), 250)
  maxYear <- index(highYear[which.max(highYear)])
  if(nrow(obj[paste0(maxYear, "::", tradeDate)]) <= 7)
  {
    str <- sprintf("DO NOT SELL: %s | [%s] Max Year [%s]", symbol, tradeDate, maxYear)
    cantSell <- c(cantSell[cantSell != str], str)
  }

  if(length(Hi(obj)[tradeDate]) == 1 &&
     (as.numeric(highYear[which.max(highYear)]) * 0.95) < as.numeric(Hi(obj)[tradeDate]))
  {
    str <- sprintf("DO NOT SELL: %s | [%s] [%s] near maximal [%s]", symbol, tradeDate, as.numeric(Hi(obj)[tradeDate]), as.numeric(highYear[which.max(highYear)]))
    cantSell <- c(cantSell[cantSell != str], str)
  }

  retValue <- c()
  retValue$canBuy  <- ifelse(is.null(cantBuy), TRUE, FALSE)
  retValue$canSell <- ifelse(is.null(cantSell), TRUE, FALSE)
  retValue$cantBuy <- cantBuy
  retValue$cantSell <- cantSell

  return(retValue)
}

fGetVolatility <- function(symbol, period)
{
  obj <- base::get(symbol)[period]
  mean(na.omit(volatility(obj)))
}

mMaxChange <- memoise(fMaxChange)
mMinChange <- memoise(fMinChange)
mBullBear <- memoise(fBullBear)
mPreventMinMax <- memoise(fPreventMinMax)
mFilterBadData <- memoise(filterBadData)
mGetVolatility <- memoise(fGetVolatility)

forgetCache <- function()
{
  forget(mMaxChange)
  forget(mMinChange)
  forget(mBullBear)
  forget(mPreventMinMax)
  forget(mFilterBadData)
  forget(singleResultM)
}

trade <- function(symbol, tradeDate, parameters = NULL, operations = NULL, price = NULL, verbose = FALSE, memoised = FALSE)
{
  if(is.null(parameters))
    return(NULL)

  canBuy <- TRUE
  canSell <- TRUE

  if(memoised)
    fData <- mFilterBadData(symbol, tradeDate)
  else
    fData <- filterBadData(symbol, tradeDate)

  alertL <- filterLRI(symbol, tradeDate, 30)

  obj <- base::get(symbol)[sprintf("/%s", tradeDate)]
  seq <- xts(as.double((Hi(obj)+Lo(obj)+Cl(obj))/3), index(obj))

  if(is.null(fData) || length(seq) < parameters$smaPeriod + 500)
    return(NULL)

  period <- tail(index(obj), 500)
  period <- paste0(first(period), "::", last(period))

  sma <- SMA(seq, parameters$smaPeriod)
  dif <- as.double(na.omit(tail(seq-sma, 500)))
  sdp <- (last(seq)-last(sma))/sd(dif)

  #sm <- smaSDdata(obj, 500, parameters$smaPeriod)
  #dif <- sm$dif
  #sma <- sm$mavg
  #sdp <- last(sm$dif/sm$sd)

  if(is.na(sdp))
  {
    warning(paste0("sdp: NA", symbol))
    return(NULL)
  }

  cantBuy <- NULL
  cantSell <- NULL

  lastValue <- as.numeric(last(Cl(obj)))

  if(memoised)
  {
    maxChange <- mMaxChange(symbol, period, lastValue)
    minChange <- mMinChange(symbol, period, lastValue)
    pMinMax <- mPreventMinMax(symbol, period)
    gVolatility <- mGetVolatility(symbol, period)
  }
  else
  {
    maxChange <- fMaxChange(symbol, period, lastValue)
    minChange <- fMinChange(symbol, period, lastValue)
    pMinMax <- fPreventMinMax(symbol, period)
    gVolatility <- fGetVolatility(symbol, period)
  }

  if(gVolatility >= 0.70)
  {
    str <- sprintf("DO NOT BUY: %s | [%s] Mean volatility too high [%.2f]", symbol, period, gVolatility)
    cantBuy <- c(cantBuy[cantBuy != str], str)
    canBuy <- FALSE
  }

  if(pMinMax$canBuy == FALSE)
    canBuy  <- FALSE

  if(pMinMax$canSell == FALSE)
    canSell <- FALSE

  cantBuy  <- unique(c(cantBuy, pMinMax$cantBuy))
  cantSell <- unique(c(cantSell, pMinMax$cantSell))

  high <- Hi(obj)
  maxValue <- as.numeric(high[which.max(high)])
  maxDate <- index(high[which.max(high)])

  lowAfter <- Lo(obj)[sprintf("%s/", maxDate)]
  minAfter <- as.numeric(lowAfter[which.min(lowAfter)])

  if(!is.na(parameters$lowLimit) && (minAfter / maxValue) < parameters$lowLimit)
  {
    str <- sprintf("DO NOT BUY: %s | [%s] Min [%f] After / Max [%s][%f] : [%f]", symbol, period, minAfter, maxDate, maxValue, (minAfter / maxValue))
    cantBuy <- c(cantBuy[cantBuy != str], str)
    canBuy <- FALSE
  }

  if(!is.na(parameters$lowLimit) && (lastValue / maxValue) < parameters$lowLimit)
  {
    str <- sprintf("DO NOT BUY: %s | [%s] Last [%f] / Max [%s][%f] : [%f]", symbol, period, lastValue, maxDate, maxValue, (lastValue / maxValue))
    cantBuy <- c(cantBuy[cantBuy != str], str)
    canBuy <- FALSE
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

  if(!is.na(parameters$bullBuy) && as.numeric(bull) < parameters$bullBuy)
  {
    str <- sprintf("DO NOT BUY: %s | [%s] Bullish [%.2f] < [%.2f]", symbol, period, bull, parameters$bullBuy)
    cantBuy <- c(cantBuy[cantBuy != str], str)
    canBuy <- FALSE
  }

  if(!is.na(parameters$bullSell) && as.numeric(bull) > parameters$bullSell)
  {
    str <- sprintf("DO NOT SELL: %s | [%s] Bullish [%.2f] > [%.2f]", symbol, period, bull, parameters$bullSell)
    cantSell <- c(cantSell[cantSell != str], str)
    canSell <- FALSE
  }

  if(!is.na(parameters$bearSell) && as.numeric(bear) < parameters$bearSell)
  {
    str <- sprintf("DO NOT SELL: %s | [%s] Bearish [%.2f] < [%.2f]", symbol, period, bear, parameters$bearSell)
    cantSell <- c(cantSell[cantSell != str], str)
    canSell <- FALSE
  }

  if(!is.na(parameters$bearBuy) && as.numeric(bear) > parameters$bearBuy)
  {
    str <- sprintf("DO NOT BUY: %s | [%s] Bearish [%.2f] > [%.2f]", symbol, period, bear, parameters$bearBuy)
    cantBuy <- c(cantBuy[cantBuy != str], str)
    canBuy <- FALSE
  }

  if(!is.na(parameters$downChange) && as.numeric(maxChange) < parameters$downChange)
  {
    str <- sprintf("DO NOT BUY: %s | [%s] Max Change : [%f]", symbol, period, maxChange)
    cantBuy <- c(cantBuy[cantBuy != str], str)
    canBuy <- FALSE
  }

  if(!is.na(parameters$upChange) && as.numeric(minChange) > parameters$upChange)
  {
    str <- sprintf("DO NOT SELL: %s | [%s] Min Change : [%f]", symbol, period, minChange)
    cantSell <- c(cantSell[cantSell != str], str)
    canSell <- FALSE
  }

  if(!is.null(alertL) && alertL != FALSE) #valor valido
  {
    if(is.null(lower) || is.na(lower))
    {
      warning(paste("lower:", lower))
    }
    if(alertL == "up" && sdp < lower) #reversao "para cima" e abaixo da banda inferior
    {
      if(canBuy)
      {
        decision <- "buy"
        reason <- sprintf("alertL == up && sdp < %1.1f -> buy", lower)
      }
    }

    if(is.null(upper) || is.na(upper))
    {
      warning(paste("upper:", upper))
    }
    else if(alertL == "down" && sdp > upper) #reversao "para baixo" e acima da banda superior
    {
      if(canSell)
      {
        decision <- "sell"
        reason <- sprintf("alertL == down && sdp > %1.1f -> sell", upper)
      }
    }
  }

  pr <- price
  if(is.null(pr) && length(operations) > 0)
  {
    opDf <- rbindlist(operations)

    if(nrow(opDf) > 0)
      opDf$price <- as.numeric(adjustOperations(symbol, xts(data.frame(price=opDf$price), order.by = opDf$tradeDate)))

    if(memoised)
      result <- singleResultM(opDf, tradeDate)
    else
      result <- singleResult(opDf, tradeDate)

    pr <- result$openMeanPrice
  }

  if(!is.null(pr))
  {
    if(!is.na(parameters$stopGain) && (pr * parameters$stopGain) <= lastValue) #Stop gain
    {
      if(canSell)
      {
        decision <- "sell"
        reason <- sprintf("Stop Gain %.2f * %2.f <= %.2f -> sell", parameters$stopGain, pr, lastValue)
      }
    }

    if(!is.na(parameters$stopLoss) && (pr * parameters$stopLoss) >= lastValue) #Stop loss
    {
      if(canSell)
      {
        decision <- "sell"
        reason <- sprintf("Stop Loss %.2f * %.2f >= %.2f -> sell", parameters$stopLoss, pr, lastValue)
      }
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
  tradeDecision$reason <- reason
  tradeDecision$canBuy <- canBuy
  tradeDecision$canSell <- canSell
  tradeDecision$price <- round(as.numeric(last(seq)), digits = 2)

  return(tradeDecision)
}
