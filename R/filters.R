
filterVolatility <- function(obj, symbol)
{
  if(nrow(obj) < 11)
  {
    print(sprintf("Insufficient data in: %s", symbol))
    return(NULL)
  }

  if(any(obj < 0))
  {
    print(sprintf("Adjust error: %s", symbol))
    return(NULL)
  }

  vol <- na.omit(diff(na.omit(volatility(obj))))

  if(max(abs(vol)) > 5)
  {
    print(sprintf("Probable adjust in %s: %s", symbol, paste(index(obj[which(na.omit(abs(diff(volatility(obj)))) > 5)]), collapse = " ")))
    return(NULL)
  }

  return(symbol)
}
