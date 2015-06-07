library("xts")
require("RMySQL", quietly = TRUE)


getSymbolsMySQL <- function (Symbols, FilterToday=FALSE, FilterAge=NULL, env = .GlobalEnv) 
{
  db.fields = c("date", "day_open", "day_high", "day_low", "day_close", "volume")

  if(FilterToday)
  {
    query <- sprintf("select distinct(symbol) from stockprices where date = date(now()) and symbol in ('%s')", paste(Symbols, collapse = "','"))
    
    fr <- getQuery(query)
    
    Symbols <- fr$symbol
  }
  
  if(!is.null(FilterAge))
  {
    query <- sprintf("select * from (select symbol, datediff(max(date),min(date)) as days from stockprices group by symbol) as age where days > %d", FilterAge)
    
    fr <- getQuery(query)
    Symbols <- intersect(Symbols, fr$symbol)
  }
  
  for (i in 1:length(Symbols))
  {
    query <- paste("SELECT ", paste(db.fields, collapse = ","), " FROM stockprices where symbol = '",  Symbols[[i]], "' ORDER BY date", sep = "")
    
    fr <- getQuery(query)
    fr <- xts(as.matrix(fr[, -1]), order.by = as.Date(fr[,1], origin = "1970-01-01"), src = "beancounter", updated = Sys.time())
    colnames(fr) <- paste(Symbols[[i]], c("Open", "High", "Low", "Close", "Volume"), sep = ".")
    
    assign(Symbols[[i]], fr, env)
  }
  
  return(Symbols)
}

getSymbolNamesMySQL <- function() 
{
  fr <- getQuery("SELECT distinct(symbol) from stockprices")

  return(fr$symbol)
}

getPositions <- function(symbol = NULL) 
{
  if(is.null(symbol))
    queryStr <- sprintf("SELECT * from positions")
  else
    queryStr <- sprintf("SELECT * from positions where symbol = '%s'", symbol)
  
  fr <- getQuery(queryStr)
  
  return(fr)
}

getWallet <- function() 
{
  fr <- getQuery("select distinct(symbol) from positions where closeVal is null")
  
  return(fr)
}

getQuery <- function(queryStr = "") 
{
  #TODO reusar conexao??
  dbConn <- dbConnect(MySQL(), default.file='mysql.config', db="beancounter")
  fr <- dbGetQuery(dbConn, queryStr)
  dbDisconnect(dbConn)
  
  return(fr)
}

