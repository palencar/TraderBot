source("dbInterface.R")

cTotal <- 0
oTotal <- 0

wallet <- getWallet()

for(symbol in wallet)
{
  
  pos <- getPositions(symbol)
  
  for(p in pos)
  {
    if(is.na(p$closeVal))
    {
      price <- p$openVal * p$size
      size <- p$size
      value <- lastPrice(symbol) * size
      proffit <- value - price
      state <- "open"
      oTotal <- oTotal + proffit
    }
    else
    {
      price <- p$openVal * p$size
      size <- p$size
      value <- p$closeVal * size
      proffit <- value - price
      state <- "closed"
      cTotal <- cTotal + proffit
    }
    
    print(paste(symbol, state, size, price, value, proffit))
  }
}

print(paste("Closed:", cTotal, "Open:", oTotal, "Total:", cTotal+oTotal))
