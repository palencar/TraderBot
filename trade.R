trade <- function(symbol, tradeDate, smaPeriod = 200, upperBand = NULL, lowerBand = NULL)
{
  allDecisions <- NULL
  i <- 1
  
  canBuy <- TRUE
  canSell <- TRUE
  alertR <- NULL
  alertL <- NULL
  
  period <- paste(rev(seq(as.Date(tradeDate), length=2, by="-2 years")),collapse = "::")
  obj <- get(symbol)[sprintf("/%s", as.Date(tradeDate))]
  
  #if(is.null(obj) || as.Date(tradeDate) %in% index(obj) == FALSE)
  #{
  #  warning(sprintf("%s %s no data", symbol, tradeDate))
  #  tradeDecision <- list()
  #  tradeDecision$decision <- "hold"
  #  tradeDecision$reason <- "bad data"
  #  return(tradeDecision)
  #}
  
  seq <- as.double((Hi(obj)+Lo(obj)+Cl(obj))/3)
  #if(length(seq) <= smaPeriod)
  #{
  #  warning(sprintf("%s %s sma(%d, %d)", symbol, tradeDate, length(seq), smaPeriod))
  #  tradeDecision <- list()
  #  tradeDecision$decision <- "hold"
  #  tradeDecision$reason <- "bad data"
  #  return(tradeDecision)
  #}

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
    sma <- SMA(seq, sPeriod)
    ssd <- sd(as.double(na.omit(seq-sma)))
    
    #alertS <- NULL
    seql = tail(seq, 2)
    smal = tail(sma, 2)
    
    #Atravessou a barreira superior, descendo
    #if(seql[2] > (smal[2] + (2*ssd)) && seql[1] <= (smal[1] + (2*ssd)))  
    #  alertS <- "upper"
    
    #Atravessou a barreira inferior, subindo
    #if(seql[2] < (smal[2] - (2*ssd)) && seql[1] >= (smal[1] - (2*ssd)))  
    #  alertS <- "lower"
    
    sdp <- (seql[2]-smal[2])/ssd
    
    #alertA <- FALSE
    #alertB <- FALSE
    
    #objLen <- length(index(obj))
    #totAb <- length(which(Hi(obj) > as.double(Hi(tail(obj, 1)))))
    #totBl <- length(which(Lo(obj) < as.double(Lo(tail(obj, 1)))))
    
    #if((totAb/objLen) < 0.1)  #Valor nos 10% superiores
    #  alertA <- totAb/objLen
    
    #if((totBl/objLen) < 0.1)  #Valor nos 10% inferiores
    #  alertB <- totBl/objLen
    
    for(lower in lowerBand)
    {
      for(upper in upperBand)
      {
        decision <- "hold"
        reason <- NULL
        
        cantBuy <- NULL
        cantSell <- NULL
        
        #ratio <- filterSMA(seq-sma) #%above/%below TODO
      
        high <- Hi(obj)
        maxValue <- as.numeric(high[which.max(high)])
        maxDate <- index(high[which.max(high)])
        
        lastValue <- as.numeric(last(Cl(obj)))

        lowYear <- Lo(obj)[sprintf("%s/", as.Date(tradeDate) - 365)]
        minYear <- index(lowYear[which.min(lowYear)]) 
        if((tradeDate - minYear) < 30)
        {
          cantBuy <- c(cantBuy, sprintf("DO NOT BUY: %s | [%s] Min Year [%s]", symbol, period, minYear))
          canBuy <- FALSE    
        }
        
        lowMonth <- Lo(obj)[sprintf("%s/", as.Date(tradeDate) - 30)]
        minDate <- index(lowMonth[which.min(lowMonth)]) 
        if((tradeDate - minDate) < 3)
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
          
        #if(ratio < 0.1)
        #{
        #  cantBuy <- c(cantBuy, sprintf("DO NOT BUY: %s | [%s] up/down: %s", symbol, period, ratio))
        #  canBuy <- FALSE
        #}
        
        #if(ratio > 0.9)
        #{
        #  cantSell <- c(cantSell, sprintf("DO NOT SELL: %s | [%s] up/down: %s", symbol, period, ratio))
        #  canSell <- FALSE
        #}
        
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
    }
  }
  
  return(allDecisions)
}
