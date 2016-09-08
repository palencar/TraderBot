trade <- function(symbol, tradeDate, upperBand = NULL, lowerBand = NULL)
{
  canBuy <- TRUE
  canSell <- TRUE
  
  period <- paste(rev(seq(as.Date(tradeDate), length=2, by="-4 years")),collapse = "::")
  obj <- get(symbol)[period]
  
  if(is.null(obj) || as.Date(tradeDate) %in% index(obj) == FALSE)
  {
    warning(sprintf("%s %s no data", symbol, tradeDate))
    tradeDecision <- list()
    tradeDecision$decision <- "hold"
    tradeDecision$reason <- "bad data"
    return(tradeDecision)
  }
  
  seq <- as.double((Hi(obj)+Lo(obj)+Cl(obj))/3)
  n <- 200
  if(length(seq) <= n)
  {
    warning(sprintf("%s %s sma(%d, %d)", symbol, tradeDate, length(seq), n))
    tradeDecision <- list()
    tradeDecision$decision <- "hold"
    tradeDecision$reason <- "bad data"
    return(tradeDecision)
  }

  alertR = tryCatch({
    filterObjectsSets(symbol, tradeDate)
  }, warning = function(war) {
    print(war)
    print(sprintf("%s %s", symbol, tradeDate))
    return(NULL)
  }, error = function(err) {
    print(err)
    print(sprintf("%s %s", symbol, tradeDate))
    return(NULL)
  }, finally={
  })    
  
  alertL = tryCatch({
    filterLRI(symbol, tradeDate)
  }, warning = function(war) {
    print(war)
    return(NULL)
  }, error = function(err) {
    print(err)
    return(NULL)
  }, finally={
  })
  
  sma <- SMA(seq, n)
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
  
  decision <- "hold"
  reason <- NULL
  
  r <- rle(sign(diff(as.vector(sma))))
  ratio <- filterSMA(r) #%up/%down

  if(ratio < 0.1)
  {
    print(sprintf("DO NOT BUY: %s | [%s] up/down: %s", symbol, period, ratio))
    canBuy <- FALSE
  }
  
  if(ratio > 0.9)
  {
    print(sprintf("DO NOT SELL: %s | [%s] up/down: %s", symbol, period, ratio))
    canSell <- FALSE
  }
  
  if(!is.null(alertR) && alertR != FALSE) #valor valido
  {
    if(canBuy && alertR == "r_up" && (is.null(lowerBand)|| sdp < lowerBand)) #reversao "para cima" e abaixo da banda inferior
    {
      decision <- "buy"
      reason <- "alertR == r_up && sdp < lowerBand -> buy"
    }
    
    if(canSell && alertR == "r_down" && (is.null(upperBand) || sdp > upperBand)) #reversao "para baixo" e acima da banda superior
    {
      decision <- "sell"
      reason <- "alertR == r_dow && sdp > upperBand -> sell"
    }
  }
  
  if(!is.null(alertL) && alertL != FALSE) #valor valido
  {
    if(canBuy && alertL == "up" && (is.null(lowerBand)|| sdp < lowerBand)) #reversao "para cima" e abaixo da banda inferior
    {
      decision <- "buy"
      reason <- "alertL == up && sdp < lowerBand -> buy"
    }
    
    if(canSell && alertL == "down" && (is.null(upperBand) || sdp > upperBand)) #reversao "para baixo" e acima da banda superior
    {
      decision <- "sell"
      reason <- "alertL == down && sdp > upperBand -> sell"
    }
  }

  tradeDecision <- list()
  tradeDecision$decision <- decision
  tradeDecision$reason <- reason
  
  return(tradeDecision)
}
