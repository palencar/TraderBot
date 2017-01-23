library("memoise")

trade <- function(symbol, tradeDate, parameters = NULL, map = NULL, price = NULL, minVol = 100000, lriTreshold = 0.6, lriPeriod = 30, verbose = FALSE)
{
  i <- 1

  if(is.null(parameters))
    return(NULL)

  allDecisions <- c(list())
  length(allDecisions) <- nrow(parameters) ^ ncol(parameters)

  canBuy <- TRUE
  canSell <- TRUE
  alertR <- NULL
  alertL <- NULL

  period <- paste(rev(seq(as.Date(tradeDate), length=2, by="-4 years")),collapse = "::")

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
    filterLRI(symbol, tradeDate, lriTreshold, lriPeriod)
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

  meanVol <- filterVolumeM(symbol, tradeDate, volume = minVol)

  for(sPeriod in parameters$smaPeriod)
  for(ll in parameters$lowLimit)
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
      str <- sprintf("DO NOT BUY: %s | [%s] Mean volume below [%d]", symbol, period, minVol)
      cantBuy <- c(cantBuy[cantBuy != str], str)
      canBuy <- FALSE
    }

    volatility <- mean(na.omit(volatility(obj)))
    if(volatility >= 0.70)
    {
      str <- sprintf("DO NOT BUY: %s | [%s] Mean volatility too high [%.2f]", symbol, period, volatility)
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
    if((tradeDate - minYear) <= 7)
    {
      str <- sprintf("DO NOT BUY: %s | [%s] Min Year [%s]", symbol, period, minYear)
      cantBuy <- c(cantBuy[cantBuy != str], str)
      canBuy <- FALSE
    }

    low2Year <- Lo(obj)[sprintf("%s/", as.Date(tradeDate) - 730)]
    min2Year <- index(low2Year[which.min(low2Year)])
    if((tradeDate - min2Year) <= 14)
    {
      str <- sprintf("DO NOT BUY: %s | [%s] Min 2 Year [%s]", symbol, period, min2Year)
      cantBuy <- c(cantBuy[cantBuy != str], str)
      canBuy <- FALSE
    }

    highYear <- Hi(obj)[sprintf("%s/", as.Date(tradeDate) - 365)]
    maxYear <- index(highYear[which.max(highYear)])
    if((tradeDate - maxYear) <= 7)
    {
      str <- sprintf("DO NOT SELL: %s | [%s] Max Year [%s]", symbol, period, maxYear)
      cantSell <- c(cantSell[cantSell != str], str)
      canSell <- FALSE
    }

    high <- Hi(obj)
    maxValue <- as.numeric(high[which.max(high)])
    maxDate <- index(high[which.max(high)])

    lowAfter <- Lo(obj)[sprintf("%s/", maxDate)]
    minAfter <- as.numeric(lowAfter[which.min(lowAfter)])

    if(!is.na(ll) && (minAfter / maxValue) < ll)
    {
      str <- sprintf("DO NOT BUY: %s | [%s] Min [%f] After / Max [%s][%f] : [%f]", symbol, period, minAfter, maxDate, maxValue, (minAfter / maxValue))
      cantBuy <- c(cantBuy[cantBuy != str], str)
      canBuy <- FALSE
    }

    if(!is.na(ll) && (lastValue / maxValue) < ll)
    {
      str <- sprintf("DO NOT BUY: %s | [%s] Last [%f] / Max [%s][%f] : [%f]", symbol, period, lastValue, maxDate, maxValue, (lastValue / maxValue))
      cantBuy <- c(cantBuy[cantBuy != str], str)
      canBuy <- FALSE
    }

    for (ub in parameters$upperBand)
    for (lb in parameters$lowerBand)
    for (dc in parameters$downChange)
    for (uc in parameters$upChange)
    for (sl in parameters$stopLoss)
    for (sg in parameters$stopGain)
    {
      decision <- "hold"
      reason <- NULL

      if(!is.na(lb))
      {
        lower <- lb + (as.numeric(maxChange))
      }

      if(!is.na(ub))
      {
        upper <- ub + (as.numeric(minChange))
      }

      if(!is.na(dc) && as.numeric(maxChange) < dc)
      {
        str <- sprintf("DO NOT BUY: %s | [%s] Max Change : [%f]", symbol, period, maxChange)
        cantBuy <- c(cantBuy[cantBuy != str], str)
        canBuy <- FALSE
      }

      if(!is.na(uc) && as.numeric(minChange) > uc)
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

      parStr <- paste(sPeriod, ub, lb, dc, uc, ll, sg, sl, collapse = " ")
      operations <- map[[parStr]]

      pr <- price
      if(is.null(pr) && !is.null(operations) && !is.na(operations))
      {
        result <- singleResultM(parStr, unlist(strsplit(operations, ";")))
        pr <- result$openMeanPrice
      }

      if(!is.null(pr))
      {
        if(!is.na(sg) && (pr * sg) <= lastValue) #Stop gain
        {
          if(canSell)
          {
            decision <- "sell"
            reason <- sprintf("Stop Gain %.2f * %2.f <= %.2f -> sell", sg, pr, lastValue)
          }
        }

        if(!is.na(sl) && (pr * sl) >= lastValue) #Stop loss
        {
          if(canSell)
          {
            decision <- "sell"
            reason <- sprintf("Stop Loss %.2f * %.2f >= %.2f -> sell", sl, pr, lastValue)
          }
        }
      }

      if(decision == "hold")
        next

      allDecisions[[i]]$decision <- decision
      allDecisions[[i]]$reason <- reason
      allDecisions[[i]]$price <- last(seq)
      pars        <- data.frame(sPeriod, ub, lb, uc, dc, ll, sg, sl)
      names(pars) <- c("smaPeriod", "upperBand", "lowerBand", "upChange", "downChange", "lowLimit", "stopGain", "stopLoss")
      allDecisions[[i]]$parameters <- pars

      i <- i + 1
    }
  }

  allDecisions <- allDecisions[!sapply(allDecisions, is.null)]

  if(verbose && !is.null(cantSell))
  {
    print(cantSell)
  }

  if(verbose && !is.null(cantBuy))
  {
    print(cantBuy)
  }

  return(allDecisions)
}
