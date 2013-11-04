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

updateProbe <- function(Symbols, date)
{
  quotes <- getQuote(Symbols)
  for(i in Symbols)
  {
    quote <- quotes[i,]
    
    if(is.na(quote$Open) || is.na(quote$High) || is.na(quote$Low) || is.na(quote$Last))
      next
    
    symbol <- get(i)
    symbol[date,1] <- quote$Open
    symbol[date,2] <- quote$High
    symbol[date,3] <- quote$Low
    symbol[date,4] <- quote$Last
    assign(i, symbol)
  }
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

loadLocalCSV <- function(symbol)
{
  queryStr <- sprintf("LOAD DATA LOCAL INFILE \'%s.csv\' INTO TABLE beancounter.stockprices_intraday FIELDS TERMINATED BY \',\' ENCLOSED BY \'\"\' LINES TERMINATED BY \'\n\' (symbol, datetime, min_open, min_low, min_high, min_close, volume)", symbol)
  return(getQuery(user = 'paulo', dbname = 'beancounter', query = queryStr)[,1])
}