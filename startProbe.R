library('quantmod')
library('multicore')
source('mysql_stocks.R')


require(compiler)
enableJIT(3)

startProbe <- function(symbolNames = NULL, update = TRUE)
{
  if(update)
    system("beancounter update today 2> /dev/null")

  if(is.null(symbolNames))
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

getQuoteDay <- function(Symbol, Day)
{
  print(sprintf("getQuoteDay [%s][%s]", Symbol, Day))
  
  modes <- c('google')
  
  for(mode in modes)
  {
    originalName <- Symbol
    if(mode == 'google')
      Symbol <- sprintf("BVMF:%s", unlist(strsplit(Symbol, "[.]"))[1])
    
    try(getSymbols(Symbol, src=mode, from=as.Date(Day), to=as.Date(Day)), silent=TRUE)
    if(exists(Symbol) == FALSE)
       return(NULL)
    
    if(is.na(Op(get(Symbol))) || is.na(Hi(get(Symbol))) ||
         is.na(Lo(get(Symbol))) || is.na(Cl(get(Symbol))) ||
         as.double(Op(get(Symbol))) == 0.0 || as.double(Hi(get(Symbol))) == 0.0 ||
         as.double(Lo(get(Symbol))) == 0.0 || as.double(Cl(get(Symbol))) == 0.0 )
      return(NULL)
    
    print(get(Symbol))
    
    table <- as.data.frame(get(Symbol))
    names(table)[1]<-paste("day_open")
    names(table)[2]<-paste("day_high")
    names(table)[3]<-paste("day_low")
    names(table)[4]<-paste("day_close")
    names(table)[5]<-paste("volume")
    table["date"] <- as.Date(Day)
    table["symbol"] <- originalName
    table[6] <- NULL

    queryStr <- sprintf("REPLACE INTO stockprices (symbol, date, day_open, day_low, day_high, day_close, volume) VALUES('%s', '%s', %f, %f, %f, %f, %g)",
                        originalName, as.Date(Day), table[1,1], table[1,2], table[1,3], table[1,4], table[1,5])
    
    getQuery(user = 'paulo', dbname = 'beancounter', query=queryStr)
  }
}

meanPrice <- function(SymbolName)
{
  return(getQuery(user = 'paulo', dbname = 'beancounter', query = sprintf("select avg(openVal) from positions where symbol = '%s' and closeVal is null", SymbolName))[,1])
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