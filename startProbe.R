library('quantmod')
library('multicore')
source('mysql_stocks.R')

require(compiler)
enableJIT(3)

startProbe <- function()
{
  system("beancounter update today 2> /dev/null")

  symbolNames <- getSymbolNamesMySQL(user = 'paulo', dbname = 'beancounter')
  
  symbolNamesObj <- getSymbolsMySQL(symbolNames, user = 'paulo', dbname = 'beancounter')
  
  return (symbolNamesObj)
}

positions <- function(symbol = NULL)
{
  if(is.null(symbol))
  {
    pos <- getPositions(user = 'paulo', dbname = 'beancounter')
  }
  else
  {
    pos <- getPositions(user = 'paulo', dbname = 'beancounter', symbol=symbol)
  }
  
  return (pos)
}

wallet <- function()
{
  wal <- getWallet(user = 'paulo', dbname = 'beancounter')

  wall <- c()
  
  if(length(wal[,1]) > 0)
  {  
    for(i in 1:length(wal[,1]))
      wall[i] <- toupper(wal[,1][i])
  }
  
  return (wall)
}

lastTradingSession <- function()
{
  return(getQuery(user = 'paulo', dbname = 'beancounter', query = "select date from stockprices order by date desc limit 1")[,1])
}