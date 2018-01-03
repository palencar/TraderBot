source("R/dbInterface.R")


#' @export
getBalance <- function(symbols = NULL, showOpen = TRUE, showClosed = FALSE, getPrices = FALSE)
{
  cTotal <- 0
  oTotal <- 0
  df <- NULL

  if(is.null(symbols))
  {
    symbols <- getWallet(showClosed)
  }

  if(getPrices)
  {
    prices <- NULL
    for(symbol in symbols)
    {
      obj <- Cl(tail(f.get.google.intraday(symbol, 60, "5d"), 1))
      prices <- rbind(prices, data.frame(row.names = symbol, Time=index(obj), Price=as.numeric(obj)))
    }
  }

  for(symbol in symbols)
  {
    pos <- getPositions(symbol)

    for(p in pos)
    {
      state <- NULL

      if(showOpen && is.na(p$closeVal))
      {
        if(getPrices)
          lp <- data.frame(datetime=prices[symbol, "Time"], close=prices[symbol, "Price"])
        else
          lp <- lastPrice(symbol)

        open <- as.Date(p$start)
        last <- lp$datetime
        price <- p$openVal * p$size
        size <- p$size
        value <- as.numeric(lp$close) * size
        profit <- value - price
        state <- "open"
        oTotal <- oTotal + profit
      }

      if(showClosed && !is.na(p$closeVal))
      {
        open <- as.Date(p$start)
        last <- as.Date(p$end)
        price <- p$openVal * p$size
        size <- p$size
        value <- p$closeVal * size
        profit <- value - price
        state <- "closed"
        cTotal <- cTotal + profit
      }

      if(!is.null(state))
      {
        df <- rbind(df, data.frame(symbol, open, last, state, size, price, value, profit, profit_p=round((profit/price)*100, digits=2)))
      }
    }
  }

  df[order(df$last, df$open), ]
}

#' @export
showBalance <- function(symbols = NULL, showOpen = TRUE, showClosed = TRUE, getPrices = FALSE)
{
  df <- getBalance(symbols, showOpen, showClosed, getPrices)

  print(df)

  oTotal <- sum(df[df$state == "open", "profit"])
  cTotal <- sum(df[df$state == "closed", "profit"])

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
