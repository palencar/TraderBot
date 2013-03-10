source('startProbe.R')

addOrder <- function(symbol, firstVal=0, lastVal=0)
{
  firstReg <- head(symbol, n=1)
  lastReg <- tail(symbol, n=1)

  xNew = xts(rep(NA,length(index(symbol))), index(symbol))

  if(firstVal == 0)
    xNew[time(firstReg)] <- as.double((Hi(firstReg)+Lo(firstReg)+Cl(firstReg))/3)
  else
    xNew[time(firstReg)] <- firstVal
  
  if(lastVal == 0)
    xNew[time(lastReg)] <- as.double((Hi(lastReg)+Lo(lastReg)+Cl(lastReg))/3)
  else
    xNew[time(lastReg)] <- lastVal

  if(as.double(xNew[time(firstReg)]) < as.double(xNew[time(lastReg)]))
    col <- 3
  else
    col <- 2
  
  legendStr <- sprintf("Buy [%s]: %.3f\tSell [%s]: %.3f",
                    time(firstReg), xNew[time(firstReg)],
                    time(lastReg), xNew[time(lastReg)])
  
  position <- xts(na.approx(xNew))
  plot(addTA(position, lwd=2, on=1, col=col))
}

addOrders <- function(symbol, name)
{
  pos <- positions(name)
  
  if(length(pos) == 0)
  {
    return()
  }
  
  for(i in 1:length(pos[,1]))
  {
    reg <- pos[i,]
    
    if(is.na(reg$end) == TRUE)
    {
      endDate <- time(tail(symbol, n=1))
    }
    else
    {
      endDate <- reg$end
    }
    
    startDate <- as.Date(reg$start)
    
    period <- sprintf("%s::%s", startDate, endDate)
    
    firstVal <- reg$openVal
    
    if(is.na(reg$closeVal) == FALSE)
    {
      addOrder(symbol[period], firstVal=firstVal, lastVal=reg$closeVal)
    }
    else
    {
      addOrder(symbol[period], firstVal=firstVal)
    }
  }
}
