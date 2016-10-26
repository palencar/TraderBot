trade <- function(symbol, tradeDate, smaPeriod = 200, upperBand = 1, lowerBand = -1, upChange = 0.5, downChange = -0.5, lowLimit = 0.6, stopGain = NULL, stopLoss = NULL, price = NULL)
{
  i <- 1

  allDecisions <- c(list())
  length(allDecisions) <- length(smaPeriod)*length(upperBand)*length(lowerBand)*
                          length(downChange)*length(upChange)*length(lowLimit)*
                          length(stopLoss)*length(stopGain)
  
  canBuy <- TRUE
  canSell <- TRUE
  alertR <- NULL
  alertL <- NULL

  period <- paste(rev(seq(as.Date(tradeDate), length=2, by="-2 years")),collapse = "::")

  alertR = tryCatch({
    filterObjectsSets(symbol, tradeDate)
  }, warning = function(war) {
    print(sprintf("filterObjectsSets: %s %s", symbol, tradeDate))
    print(war)
    return(NULL)
  }, error = function(err) {
    print(sprintf("filterObjectsSets: %s %s", symbol, tradeDate))
    print(err)
    return(NULL)
  }, finally={
  })    
  
  alertL = tryCatch({
    filterLRI(symbol, tradeDate)
  }, warning = function(war) {
    print(sprintf("filterLRI: %s %s", symbol, tradeDate))
    print(war)
    return(NULL)
  }, error = function(err) {
    print(sprintf("filterLRI: %s %s", symbol, tradeDate))
    print(err)
    return(NULL)
  }, finally={
  })
  
  meanVol <- filterVolume(symbol, tradeDate, volume = 500000)
  
  for(sPeriod in smaPeriod)
  for(ll in lowLimit)
  {
    #compute sd for the period
    objPeriod <- get(symbol)[period]
    seq <- as.double((Hi(objPeriod)+Lo(objPeriod)+Cl(objPeriod))/3)
    sma <- SMA(seq, sPeriod)
    ssd <- sd(as.double(na.omit(seq-sma)))
    
    #compute sma for all the data
    obj <- get(symbol)[sprintf("/%s", as.Date(tradeDate))]
    seq <- as.double((Hi(obj)+Lo(obj)+Cl(obj))/3)
    sma <- SMA(seq, sPeriod)
    
    seql = tail(seq, 2)
    smal = tail(sma, 2)
    
    sdp <- (seql[2]-smal[2])/ssd

    cantBuy <- NULL
    cantSell <- NULL
    
    if(is.null(meanVol))
    {
      str <- sprintf("DO NOT BUY: %s | [%s] Mean volume below [%d]", symbol, period, 500000)
      cantBuy <- c(cantBuy[cantBuy != str], str)
      canBuy <- FALSE
    }

    lastValue <- as.numeric(last(Cl(obj)))
    
    high <- Hi(objPeriod)[sprintf("/%s", as.Date(tradeDate) - 30)] #Considera ate um mes atras
    maxValue <- as.numeric(high[which.max(high)])
    maxDate <- index(high[which.max(high)])
    
    lr <- linearRegression(Cl(objPeriod[sprintf("%s/%s", maxDate, tradeDate)]))
    maxChange <- as.numeric((lr$coef*365)/lastValue)
    if(is.na(maxChange))
    {
      maxChange <- 0
    }
    
    low <- Lo(objPeriod)[sprintf("/%s", as.Date(tradeDate) - 30)] #Considera ate um mes atras
    minValue <- as.numeric(low[which.min(low)])
    minDate <- index(low[which.min(low)])
    
    lr <- linearRegression(Cl(objPeriod[sprintf("%s/%s", minDate, tradeDate)]))
    minChange <- as.numeric((lr$coef*365)/lastValue)
    if(is.na(minChange))
    {
      minChange <- 0
    }
    
    lowYear <- Lo(obj)[sprintf("%s/", as.Date(tradeDate) - 365)]
    minYear <- index(lowYear[which.min(lowYear)]) 
    if((tradeDate - minYear) < 30)
    {
      str <- sprintf("DO NOT BUY: %s | [%s] Min Year [%s]", symbol, period, minYear)
      cantBuy <- c(cantBuy[cantBuy != str], str)
      canBuy <- FALSE    
    }
    
    lowMonth <- Lo(obj)[sprintf("%s/", as.Date(tradeDate) - 30)]
    minDate <- index(lowMonth[which.min(lowMonth)]) 
    if((tradeDate - minDate) == 0)
    {
      str <- sprintf("DO NOT BUY: %s | [%s] Min Month [%s]", symbol, period, minDate)
      cantBuy <- c(cantBuy[cantBuy != str], str)
      canBuy <- FALSE    
    }
    
    high <- Hi(obj)
    maxValue <- as.numeric(high[which.max(high)])
    maxDate <- index(high[which.max(high)])
    
    lowAfter <- Lo(obj)[sprintf("%s/", maxDate)]
    minAfter <- as.numeric(lowAfter[which.min(lowAfter)])

    if((minAfter / maxValue) < ll)
    {
      str <- sprintf("DO NOT BUY: %s | [%s] Min [%f] After / Max [%s][%f] : [%f]", symbol, period, minAfter, maxDate, maxValue, (minAfter / maxValue))
      cantBuy <- c(cantBuy[cantBuy != str], str)
      canBuy <- FALSE
    }
    
    if((lastValue / maxValue) < ll) #below 60% low
    {
      str <- sprintf("DO NOT BUY: %s | [%s] Last [%f] / Max [%s][%f] : [%f]", symbol, period, lastValue, maxDate, maxValue, (lastValue / maxValue))
      cantBuy <- c(cantBuy[cantBuy != str], str)
      canBuy <- FALSE
    }
    
    for (ub in upperBand)
    for (lb in lowerBand)
    for (dc in downChange)
    for (uc in upChange)
    for (sl in stopLoss)
    for (sg in stopGain)
    {
      decision <- "hold"
      reason <- NULL
      
      if(!is.null(lowerBand))
      {
        lower <- lb + (as.numeric(maxChange))
      }
      
      if(!is.null(upperBand))
      {
        upper <- ub + (as.numeric(minChange))
      }
      
      if(as.numeric(maxChange) < dc)
      {
        str <- sprintf("DO NOT BUY: %s | [%s] Max Change : [%f]", symbol, period, maxChange)
        cantBuy <- c(cantBuy[cantBuy != str], str)
        canBuy <- FALSE
      }
      
      if(as.numeric(minChange) > uc)
      {
        str <- sprintf("DO NOT SELL: %s | [%s] Min Change : [%f]", symbol, period, minChange)
        cantSell <- c(cantSell[cantSell != str], str)
        canSell <- FALSE
      }
  
      if(!is.null(alertR) && alertR != FALSE) #valor valido
      {
        if(alertR == "r_up" && (is.null(lower)|| sdp < lower)) #reversao "para cima" e abaixo da banda inferior
        {
          if(canBuy)
          {
            decision <- "buy"
            reason <- sprintf("alertR == r_up && sdp < %1.1f -> buy", lower)
          }
        }
        
        if(alertR == "r_down" && (is.null(upper) || sdp > upper)) #reversao "para baixo" e acima da banda superior
        {
          if(canSell)
          {
            decision <- "sell"
            reason <- sprintf("alertR == r_dow && sdp > %1.1f -> sell", upper)
          }
        }
      }
      
      if(!is.null(alertL) && alertL != FALSE) #valor valido
      {
        if(alertL == "up" && (is.null(lower)|| sdp < lower)) #reversao "para cima" e abaixo da banda inferior
        {
          if(canBuy)
          {
            decision <- "buy"
            reason <- sprintf("alertL == up && sdp < %1.1f -> buy", lower)
          }
        }
        
        if(alertL == "down" && (is.null(upper) || sdp > upper)) #reversao "para baixo" e acima da banda superior
        {
          if(canSell)
          {
            decision <- "sell"
            reason <- sprintf("alertL == down && sdp > %1.1f -> sell", upper)
          }
        }
      }
      
      if(!is.null(price))
      {
        if(!is.null(sg) && (price * sg) <= lastValue) #Stop gain
        {
          if(canSell)
          {
            decision <- "sell"
            reason <- sprintf("Stop Gain %.2f * %2.f <= %.2f -> sell", sg, price, lastValue)
          }
        }
        
        if(!is.null(sl) && (price * sl) >= lastValue) #Stop loss
        {
          if(canSell)
          {
            decision <- "sell"
            reason <- sprintf("Stop Loss %.2f * %.2f >= %.2f -> sell", sl, price, lastValue)
          }
        }
      }
      
      if(decision == "hold")
        next
      
      allDecisions[[i]]$decision <- decision
      allDecisions[[i]]$reason <- reason
      allDecisions[[i]]$parameters <- c(sPeriod, ub, lb, dc, uc, ll, sg, sl)
      allDecisions[[i]]$price <- last(seq)
      
      i <- i + 1
    }
  }
  
  allDecisions <- allDecisions[!sapply(allDecisions, is.null)]
  
  if(!is.null(cantSell))
  {
    print(cantSell)
  }
  
  if(!is.null(cantBuy))
  {
    print(cantBuy)
  }
  
  return(allDecisions)
}
