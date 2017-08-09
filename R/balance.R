source("R/dbInterface.R")

#' @export
showBalance <- function(symbols = NULL, showOpen = TRUE, showClosed = FALSE)
{
  cTotal <- 0
  oTotal <- 0
  df <- NULL

  if(is.null(symbols))
  {
    symbols <- getWallet(showClosed)
  }

  for(symbol in symbols)
  {
    pos <- getPositions(symbol)

    for(p in pos)
    {
      state <- NULL

      if(showOpen && is.na(p$closeVal))
      {
        price <- p$openVal * p$size
        size <- p$size
        value <- lastPrice(symbol) * size
        proffit <- value - price
        state <- "open"
        oTotal <- oTotal + proffit
      }

      if(showClosed && !is.na(p$closeVal))
      {
        price <- p$openVal * p$size
        size <- p$size
        value <- p$closeVal * size
        proffit <- value - price
        state <- "closed"
        cTotal <- cTotal + proffit
      }

      if(!is.null(state))
      {
        df <- rbind(df, data.frame(symbol, state, size, price, value, proffit))
      }
    }
  }

  print(df)

  if(showOpen && showClosed)
  {
    print(paste("Closed:", cTotal, "Open:", oTotal, "Total:", cTotal+oTotal))
  }
  else if(showOpen)
  {
    print(paste("Open:", oTotal))
  }
  else if(showClosed)
  {
    print(paste("Closed:", cTotal))
  }
}
