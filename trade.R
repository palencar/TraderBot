trade <- function(symbol, tradeDate, smaPeriod = 200, upperBand = NULL, lowerBand = NULL)
{
  allDecisions <- NULL
  i <- 1
  
  canBuy <- TRUE
  canSell <- TRUE
  alertR <- NULL
  alertL <- NULL
  
  print(paste(symbol, tradeDate))
  
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
  
  for(sPeriod in smaPeriod)
  {
    #compute sd for the period
    obj <- get(symbol)[period]
    seq <- as.double((Hi(obj)+Lo(obj)+Cl(obj))/3)
    sma <- SMA(seq, sPeriod)
    ssd <- sd(as.double(na.omit(seq-sma)))
    
    #compute sma for all the data
    obj <- get(symbol)[sprintf("/%s", as.Date(tradeDate))]
    seq <- as.double((Hi(obj)+Lo(obj)+Cl(obj))/3)
    sma <- SMA(seq, sPeriod)
    
    alertS <- NULL
    seql = tail(seq, 2)
    smal = tail(sma, 2)
    
    sdp <- (seql[2]-smal[2])/ssd

    decision <- "hold"
    reason <- NULL
    cantBuy <- NULL
    cantSell <- NULL

    lastValue <- as.numeric(last(Cl(obj)))
    
    high <- Hi(obj)
    maxValue <- as.numeric(high[which.max(high)])
    maxDate <- index(high[which.max(high)])
    
    lr <- linearRegression(Cl(obj[sprintf("%s/%s", maxDate, tradeDate)]))
    maxChange <- as.numeric((lr$coef*365)/lastValue)
    if(is.na(maxChange))
    {
      maxChange <- 0
    }
    
    low <- Lo(obj)
    minValue <- as.numeric(low[which.min(low)])
    minDate <- index(low[which.min(low)])
    
    lr <- linearRegression(Cl(obj[sprintf("%s/%s", minDate, tradeDate)]))
    minChange <- as.numeric((lr$coef*365)/lastValue)
    if(is.na(minChange))
    {
      minChange <- 0
    }

    lower <- lowerBand + (as.numeric(maxChange))
    upper <- upperBand + (as.numeric(minChange))

    lowYear <- Lo(obj)[sprintf("%s/", as.Date(tradeDate) - 365)]
    minYear <- index(lowYear[which.min(lowYear)]) 
    if((tradeDate - minYear) < 30)
    {
      cantBuy <- c(cantBuy, sprintf("DO NOT BUY: %s | [%s] Min Year [%s]", symbol, period, minYear))
      canBuy <- FALSE    
    }
    
    lowMonth <- Lo(obj)[sprintf("%s/", as.Date(tradeDate) - 30)]
    minDate <- index(lowMonth[which.min(lowMonth)]) 
    if((tradeDate - minDate) == 0)
    {
      cantBuy <- c(cantBuy, sprintf("DO NOT BUY: %s | [%s] Min Month [%s]", symbol, period, minDate))
      canBuy <- FALSE    
    }
    
    lowAfter <- Lo(obj)[sprintf("%s/", maxDate)]
    minAfter <- as.numeric(lowAfter[which.min(lowAfter)])
    
    if((minAfter / maxValue) < 0.6)
    {
      cantBuy <- c(cantBuy, sprintf("DO NOT BUY: %s | [%s] Min [%f] After / Max [%s][%f] : [%f]", symbol, period, minAfter, maxDate, maxValue, (minAfter / maxValue)))
      canBuy <- FALSE          
    }
    
    if((lastValue / maxValue) < 0.6) #below 60% low
    {
      cantBuy <- c(cantBuy, sprintf("DO NOT BUY: %s | [%s] Last [%f] / Max [%s][%f] : [%f]", symbol, period, lastValue, maxDate, maxValue, (lastValue / maxValue)))
      canBuy <- FALSE
    }
    
    if(as.numeric(maxChange) < -1.0)
    {
      cantBuy <- c(cantBuy, sprintf("DO NOT BUY: %s | [%s] year change : [%f]", symbol, period, maxChange))
      canBuy <- FALSE
    }
    
    if(as.numeric(minChange) > 1.0)
    {
      cantSell <- c(cantBuy, sprintf("DO NOT SELL: %s | [%s] year change : [%f]", symbol, period, minChange))
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
        else
        {
          print(cantBuy)
        }
      }
      
      if(alertR == "r_down" && (is.null(upper) || sdp > upper)) #reversao "para baixo" e acima da banda superior
      {
        if(canSell)
        {
          decision <- "sell"
          reason <- sprintf("alertR == r_dow && sdp > %1.1f -> sell", upper)
        }
        else
        {
          print(cantSell)
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
        else
        {
          print(cantBuy)
        }
      }
      
      if(alertL == "down" && (is.null(upper) || sdp > upper)) #reversao "para baixo" e acima da banda superior
      {
        if(canSell)
        {
          decision <- "sell"
          reason <- sprintf("alertL == down && sdp > %1.1f -> sell", upper)
        }
        else
        {
          print(cantSell)
        }
      }
    }
  
    tradeDecision <- list()
    tradeDecision$decision <- decision
    tradeDecision$reason <- reason
    tradeDecision$parameters <- c(sPeriod, upper, lower)
    
    allDecisions[[i]] <- tradeDecision
    i <- i + 1
  }
  
  return(allDecisions)
}
