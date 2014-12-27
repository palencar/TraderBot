library("xts")
require("RMySQL", quietly = TRUE)

dbConnnection <- function()
{
  if(exists("con") && isIdCurrent(con))
    return(con)
  
  con <- dbConnect(MySQL(), default.file='mysql.config', db="beancounter")
  
  return(con)
}

getSymbolsMySQL <- function (Symbols, FilterToday=FALSE, FilterAge=NULL, env = .GlobalEnv) 
{
  db.fields = c("date", "day_open", "day_high", "day_low", "day_close", "volume")

  con <- dbConnnection()
  
  if(FilterToday)
  {
    query <- sprintf("select distinct(symbol) from stockprices where date = date(now()) and symbol in ('%s')", paste(Symbols, collapse = "','"))
    rs <- dbSendQuery(con, query)
    fr <- fetch(rs, n = -1)
    Symbols <- fr$symbol
  }
  
  if(!is.null(FilterAge))
  {
    query <- sprintf("select * from (select symbol, datediff(max(date),min(date)) as days from stockprices group by symbol) as age where days > %d", FilterAge)
    rs <- dbSendQuery(con, query)
    fr <- fetch(rs, n = -1)
    Symbols <- intersect(Symbols, fr$symbol)
  }
  
  for (i in 1:length(Symbols))
  {
    query <- paste("SELECT ", paste(db.fields, collapse = ","), " FROM stockprices where symbol = '",  Symbols[[i]], "' ORDER BY date", sep = "")
    
    rs <- dbSendQuery(con, query)
    fr <- fetch(rs, n = -1)
    fr <- xts(as.matrix(fr[, -1]), order.by = as.Date(fr[,1], origin = "1970-01-01"), src = "beancounter", updated = Sys.time())
    colnames(fr) <- paste(Symbols[[i]], c("Open", "High", "Low", "Close", "Volume"), sep = ".")
    
    assign(Symbols[[i]], fr, env)
  }
  
  dbDisconnect(con)
  
  return(Symbols)
}

getSymbolNamesMySQL <- function() 
{
  con <- dbConnnection()
  
  rs <- dbSendQuery(con, "SELECT distinct(symbol) from stockprices")
  fr <- fetch(rs, n = -1)

  dbDisconnect(con)
  
  return(fr$symbol)
}

getPositions <- function(symbol = NULL) 
{  
  con <- dbConnnection()
  
  if(is.null(symbol))
    queryStr <- sprintf("SELECT * from positions")
  else
    queryStr <- sprintf("SELECT * from positions where symbol = '%s'", symbol)
  
  rs <- dbSendQuery(con, queryStr)
  fr <- fetch(rs, n = -1)
  
  dbDisconnect(con)
  
  return(fr)
}

getWallet <- function() 
{
  con <- dbConnnection()
   
  rs <- dbSendQuery(con, "select distinct(symbol) from positions")
  fr <- fetch(rs, n = -1)
  
  dbDisconnect(con)
  
  return(fr)
}

getQuery <- function(queryStr = "") 
{
  con <- dbConnnection()

  fr <- dbGetQuery(con, queryStr)
  
  dbDisconnect(con)
  
  return(fr)
}

