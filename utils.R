library(xts)
library(pastecs)

turnPoints <- function(object, maxTpoints=3)
{
  sigmas <- c()
  for(i in 1:length(object))
  {
    reg <- object[[i]]
    sigmas[[i]] <- reg$sigma
    #print(reg$sigma)
  }
  
  if(length(sigmas) < maxTpoints)
    return(sigmas)
    
  tp <- extract(turnpoints(sigmas), 100000, peak=0, pit=1)
  tPoints <- c()
  
  Tp <- object[[length(tp)]]$period
  for(i in length(tp):1)
  {
    if(tp[i] == 1)
    {
      tPoints[[i]] <- object[[i]]$sigma
      Tp <- object[[i]]$sigma
    }
    else
    {
      tPoints[[i]] <- Tp
    }
  }
  
  if(length(sigmas) < maxTpoints)
    return(sigmas)
  
  tp <- extract(turnpoints(tPoints), 100000, peak=0, pit=1)
  
  k <- 1
  lista <- c()
  
  for(i in 1:length(tp))
  {
    if(tp[i] == 1)
    {
      #plotPolyReg(object[[i]]$name, object[[i]]$regression, object[[i]]$sigma)
      #Sys.sleep(2)
      lista[[k]] <- object[[i]]
      k <- k+1
    }
  }
  
  return(lista)
}