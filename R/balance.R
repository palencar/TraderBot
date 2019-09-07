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
      upd <- tail(f.get.google.intraday(symbol, 60, "5d"), 1)
      obj <- xts(NA, order.by = Sys.time())

      if(nrow(upd) == 1)
        obj <- Cl(upd)

      prices <- rbind(prices, data.frame(row.names = symbol, Time=index(obj), Price=as.numeric(obj)))
    }
  }

  for(symbol in symbols)
  {
    pos <- getPositions(symbol)
    spl <- getSplits.db(symbol)

    for(p in pos)
    {
      state <- NULL

      s <- spl
      if(!is.na(p$end))
        s <- s[index(s) <= p$end]

      if(any(index(s) > p$start))
        adj.size <- p$size / prod(s[index(s) > p$start])
      else
        adj.size <- p$size

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
        value <- as.numeric(lp$close) * adj.size
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
        value <- p$closeVal * adj.size
        profit <- value - price
        state <- "closed"
        cTotal <- cTotal + profit
      }

      if(!is.null(state))
      {
        df <- rbind(df, data.frame(symbol, open, last, state, size, price, value, profit, profit_perc=round((profit/price)*100, digits=2), adj.size))
      }
    }
  }

  if(is.null(df))
    return(NULL)

  df <- as.data.table(df[order(df$last, df$open), ])
  df[, adj.price :={
    op <- rbind(xts(price/size, order.by = as.POSIXct(as.Date(open))), xts(as.numeric(value/size), order.by = as.POSIXct(last)))
    round(as.numeric(adjustOperations(as.character(symbol), op)[as.POSIXct(open)])*adj.size, digits = 2)
  } , by=symbol]
  df$adj.profit <- df$value-df$adj.price
  df$adj.profit_perc <- round((df$adj.profit*100)/df$adj.price, 2)
  df
}

#' @export
showBalance <- function(symbols = NULL, showOpen = TRUE, showClosed = FALSE, getPrices = FALSE)
{
  df <- as.data.frame(getBalance(symbols, showOpen, showClosed, getPrices))

  print(df)

  print("Total:")

  Open <- sum(df[df$state == "open", "profit"])
  Closed <- sum(df[df$state == "closed", "profit"])
  Adj.Open <- sum(df[df$state == "open", "adj.profit"])
  Adj.Closed <- sum(df[df$state == "closed", "adj.profit"])

  total <- NULL

  if(showClosed)
    total <- cbind(Closed, Adj.Closed)

  if(showOpen)
    total <- cbind(total, Open, Adj.Open)

  if(nrow(total) > 0)
    print(total)
}
