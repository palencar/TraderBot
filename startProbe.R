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
    pos <- getPositions(user = 'paulo', dbname = 'beancounter');
  }
  else
  {
    pos <- getPositions(user = 'paulo', dbname = 'beancounter', symbol=symbol);
  }
  
  return (pos)
}
