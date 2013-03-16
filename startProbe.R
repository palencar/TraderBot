library('quantmod')
source('mysql_stocks.R')


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