trade <- function(symbol, tradeDate)
{
  period <- paste(rev(seq(as.Date(tradeDate), length=2, by="-4 years")),collapse = "::")

  alertR = tryCatch({
    computeRegressions(symbol, as.Date(tradeDate), as.Date(tradeDate))
  }, warning = function(war) {
    print(war)
    return(NULL)
  }, error = function(err) {
    print(err)
    return(NULL)
  }, finally={
  })    
  
  alertL = tryCatch({
    filterLRI(linearRegressionIndicator(symbol, get(symbol)[period])[period])
  }, warning = function(war) {
    print(war)
    return(NULL)
  }, error = function(err) {
    print(err)
    return(NULL)
  }, finally={
  })
  
  obj <- get(symbol)[period]
  seq <- as.double((Hi(obj)+Lo(obj)+Cl(obj))/3)
  sma <- SMA(seq, n=200)
  ssd <- sd(as.double(na.omit(seq-sma)))
  
  alertS <- FALSE
  seql = tail(seq, 2)
  smal = tail(sma, 2)
  
  #Atravessou a barreira superior, descendo
  if(seql[2] > (smal[2] + (2*ssd)) && seql[1] <= (smal[1] + (2*ssd)))  
    alertS <- "upper"
  
  #Atravessou a barreira inferior, subindo
  if(seql[2] < (smal[2] - (2*ssd)) && seql[1] >= (smal[1] - (2*ssd)))  
    alertS <- "lower"
  
  sdp <- (seql[2]-smal[2])/ssd
  
  alertA <- FALSE
  alertB <- FALSE

  objLen <- length(index(obj))
  totAb <- length(which(Hi(obj) > as.double(Hi(tail(obj, 1)))))
  totBl <- length(which(Lo(obj) < as.double(Lo(tail(obj, 1)))))
  
  if((totAb/objLen) < 0.1)  #Valor nos 10% superiores
    alertA <- totAb/objLen
  
  if((totBl/objLen) < 0.1)  #Valor nos 10% inferiores
    alertB <- totBl/objLen
  
  lsma <- last(SMA(as.double((Hi(obj)+Lo(obj)+Cl(obj))/3), n=200))
  lst <- last(as.double((Hi(obj)+Lo(obj)+Cl(obj))/3))
  
  decision <- "hold"
  reason <- NULL
  
  if(!is.null(alertR) && alertR != FALSE) #valor valido
  {
    if(alertR == "r_up" && sdp < -1.0) #reversao "para cima" e abaixo de -1x o desvio padrao
    {
      decision <- "buy"
      reason <- "alertR == r_up && sdp < -1.0 -> buy"
    }
    
    if(alertR == "r_down" && sdp > 0.0) #reversao "para baixo" e acima da media movel
    {
      decision <- "sell"
      reason <- "alertR == r_dow && sdp > 0.0 -> sell"
    }
  }
  
  if(!is.null(alertL) && alertL != FALSE) #valor valido
  {
    if(alertL == "up" && sdp < -1.0) #reversao "para cima" e abaixo de -1x o desvio padrao
    {
      decision <- "buy"
      reason <- "alertL == up && sdp < -1.0 -> buy"
    }
    
    if(alertL == "down" && sdp > 0.0) #reversao "para baixo" e acima da media movel
    {
      decision <- "sell"
      reason <- "alertL == down && sdp > 0.0 -> sell"
    }
  }
  
  tradeDecision <- list()
  tradeDecision$decision <- decision
  tradeDecision$reason <- reason
  
  return(tradeDecision)
}
