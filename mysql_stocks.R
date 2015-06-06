library("xts")
require("RMySQL", quietly = TRUE)

dbConnnection <- function()
{
  if(exists("dbMysqlCon_") && isIdCurrent(dbMysqlCon_))
    return(dbMysqlCon_)
  
  dbMysqlCon_ <- dbConnect(MySQL(), default.file='mysql.config', db="beancounter")
  
  return(dbMysqlCon_)
}

getSymbolsMySQL <- function (Symbols, FilterToday=FALSE, FilterAge=NULL, env = .GlobalEnv) 
{
  db.fields = c("date", "day_open", "day_high", "day_low", "day_close", "volume")

  dbMysqlCon_ <- dbConnnection()
  
  if(FilterToday)
  {
    query <- sprintf("select distinct(symbol) from stockprices where date = date(now()) and symbol in ('%s')", paste(Symbols, collapse = "','"))
    #rs <- dbSendQuery(dbMysqlCon_, query)
    #fr <- fetch(rs, n = -1)
    fr <- dbGetQuery(dbMysqlCon_, query)
    
    Symbols <- fr$symbol
  }
  
  if(!is.null(FilterAge))
  {
    query <- sprintf("select * from (select symbol, datediff(max(date),min(date)) as days from stockprices group by symbol) as age where days > %d", FilterAge)
    #rs <- dbSendQuery(dbMysqlCon_, query)
    #fr <- fetch(rs, n = -1)
    fr <- dbGetQuery(dbMysqlCon_, query)
    Symbols <- intersect(Symbols, fr$symbol)
  }
  
  for (i in 1:length(Symbols))
  {
    query <- paste("SELECT ", paste(db.fields, collapse = ","), " FROM stockprices where symbol = '",  Symbols[[i]], "' ORDER BY date", sep = "")
    
    #rs <- dbSendQuery(dbMysqlCon_, query)
    #fr <- fetch(rs, n = -1)
    fr <- dbGetQuery(dbMysqlCon_, query)
    fr <- xts(as.matrix(fr[, -1]), order.by = as.Date(fr[,1], origin = "1970-01-01"), src = "beancounter", updated = Sys.time())
    colnames(fr) <- paste(Symbols[[i]], c("Open", "High", "Low", "Close", "Volume"), sep = ".")
    
    assign(Symbols[[i]], fr, env)
  }
  
  #dbDisconnect(dbMysqlCon_)
  
  return(Symbols)
}

getSymbolNamesMySQL <- function() 
{
  dbMysqlCon_ <- dbConnnection()
  
  #rs <- dbSendQuery(dbMysqlCon_, "SELECT distinct(symbol) from stockprices")
  #fr <- fetch(rs, n = -1)
  fr <- dbGetQuery(dbMysqlCon_, "SELECT distinct(symbol) from stockprices")

  #dbDisconnect(dbMysqlCon_)
  
  return(fr$symbol)
}

getPositions <- function(symbol = NULL) 
{  
  dbMysqlCon_ <- dbConnnection()
  
  if(is.null(symbol))
    queryStr <- sprintf("SELECT * from positions")
  else
    queryStr <- sprintf("SELECT * from positions where symbol = '%s'", symbol)
  
  #rs <- dbSendQuery(dbMysqlCon_, queryStr)
  #fr <- fetch(rs, n = -1)
  fr <- dbGetQuery(dbMysqlCon_, queryStr)
  
  #dbDisconnect(dbMysqlCon_)
  
  return(fr)
}

getWallet <- function() 
{
  dbMysqlCon_ <- dbConnnection()
   
  #rs <- dbSendQuery(dbMysqlCon_, "select distinct(symbol) from positions where closeVal is null")
  #fr <- fetch(rs, n = -1)
  fr <- dbGetQuery(dbMysqlCon_, "select distinct(symbol) from positions where closeVal is null")
  
  #dbDisconnect(dbMysqlCon_)
  
  return(fr)
}

getQuery <- function(queryStr = "") 
{
  dbMysqlCon_ <- dbConnnection()

  fr <- dbGetQuery(dbMysqlCon_, queryStr)
  
  #dbDisconnect(dbMysqlCon_)
  
  return(fr)
}

