trade <- function(symbol, tradeDate)
{
  decision <- "hold"
  
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
    filterLRI(linearRegressionIndicator(symbol)[sprintf("/%s", as.Date(tradeDate))], threshold=1.2)
  }, warning = function(war) {
    print(war)
    return(NULL)
  }, error = function(err) {
    print(err)
    return(NULL)
  }, finally={
  })
  
  obj <- get(symbol)[sprintf("/%s", as.Date(tradeDate))]
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
  
  #TODO utilizar valor Hi e Lo em vez da media
  sdp <- (seql[2]-smal[2])/ssd
  
  alertA <- FALSE
  alertB <- FALSE
  #TODO utilizar valor Hi e Lo em vez da media
  objOHLC <- obj[paste(rev(seq(as.Date(tradeDate), length=2, by="-4 years")),collapse = "::")]
  objLen <- length(index(objOHLC))
  totAb <- length(which(Hi(objOHLC) > as.double(Hi(tail(obj, 1)))))
  totBl <- length(which(Lo(objOHLC) < as.double(Lo(tail(obj, 1)))))
  
  if((totAb/objLen) < 0.1)  #Valor nos 10% superiores
    alertA <- totAb/objLen
  
  if((totBl/objLen) < 0.1)  #Valor nos 10% inferiores
    alertB <- totBl/objLen
  
  #if(!is.null(alertR))
  #  print(sprintf("%s %s: alertR %s", as.Date(tradeDate), symbol, alertR))
  
  #if(!is.null(alertL))
  #  print(sprintf("%s %s: alertL %s", as.Date(tradeDate), symbol, alertL))
  
  #print(sprintf("%s %s: alertS %s", as.Date(tradeDate), symbol, alertS))
  
  #if(alertA != FALSE)
  #  print(sprintf("%s %s: alertA %s", as.Date(tradeDate), symbol, alertA))
  
  #if(alertB != FALSE)
  #  print(sprintf("%s %s: alertB %s", as.Date(tradeDate), symbol, alertB))
  
  #logLine <- paste(as.Date(tradeDate), paste(as.double(tail(obj, 1)), collapse = " "), alertR, alertL, alertS, alertA, alertB, sdp)
  #logFile <- paste("training/",symbol,".log", sep = "")
  #cat(logLine, file=logFile, sep = "\n", append=TRUE)
  
  obj <- get(symbol)
  lsma <- last(SMA(as.double((Hi(obj)+Lo(obj)+Cl(obj))/3), n=200))
  lst <- last(as.double((Hi(obj)+Lo(obj)+Cl(obj))/3))
  
  if(!is.null(alertR) && alertR != FALSE) #valor valido
  {
    if(alertR == "r_up" && sdp < -1.0) #reversao "para cima" e abaixo de -1x o desvio padrao
    {
      decision <- "buy"
    }
    
    if(alertR == "r_down" && sdp > 1.0) #reversao "para baixo" e acima de 1x o desvio padrao
    {
      decision <- "sell"
    }
  }
  
  if(!is.null(alertL) && alertL != FALSE) #valor valido
  {
    if(alertL == "up" && sdp < -1.0) #reversao "para cima" e abaixo de -1x o desvio padrao
    {
      decision <- "buy"
    }
    
    if(alertL == "down" && sdp > 1.0) #reversao "para baixo" e acima de 1x o desvio padrao
    {
      decision <- "sell"
    }
  }
  
  #TODO
  #talvez utilizar os indicadores alertA, alertB e alertS em para auxiliar na decisao de outro indicador
  #NUNCA utilizar os indicadores isoladamente
  
  #alertLog <- paste(alertLog, paste(symbol, logLine, collapse = " "), sep = "\n")
  
  return(decision)
}