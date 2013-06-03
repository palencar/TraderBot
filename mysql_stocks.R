convert.time.series <- function (fr, return.class) 
{
  if ("quantmod.OHLC" %in% return.class) {
    class(fr) <- c("quantmod.OHLC", "zoo")
    return(fr)
  }
  else if ("xts" %in% return.class) {
    return(fr)
  }
  if ("zoo" %in% return.class) {
    return(as.zoo(fr))
  }
  else if ("ts" %in% return.class) {
    fr <- as.ts(fr)
    return(fr)
  }
  else if ("data.frame" %in% return.class) {
    fr <- as.data.frame(fr)
    return(fr)
  }
  else if ("matrix" %in% return.class) {
    fr <- as.data.frame(fr)
    return(fr)
  }
  else if ("its" %in% return.class) {
    if ("package:its" %in% search() || suppressMessages(require("its", 
                                                                quietly = TRUE))) {
      fr.dates <- as.POSIXct(as.character(index(fr)))
      fr <- its::its(coredata(fr), fr.dates)
      return(fr)
    }
    else {
      warning(paste("'its' from package 'its' could not be loaded:", 
                    " 'xts' class returned"))
    }
  }
  else if ("timeSeries" %in% return.class) {
    if ("package:timeSeries" %in% search() || suppressMessages(require("timeSeries", 
                                                                       quietly = TRUE))) {
      fr <- timeSeries(coredata(fr), charvec = as.character(index(fr)))
      return(fr)
    }
    else {
      warning(paste("'timeSeries' from package 'timeSeries' could not be loaded:", 
                    " 'xts' class returned"))
    }
  }
}

getSymbolsMySQL <- function (Symbols, env = .GlobalEnv, user = NULL, 
                                password = NULL, dbname = NULL, ...) 
{
  return.class = "xts"
  db.fields = c("date", "day_open", "day_high", "day_low", "day_close", "volume")

  this.env <- environment()
  for (var in names(list(...))) {
    assign(var, list(...)[[var]], this.env)
  }
  #if (missing(verbose)) 
    verbose <- FALSE
  #if (missing(auto.assign)) 
    auto.assign <- TRUE
  if (require("DBI", quietly = TRUE)) {
    if ("package:RMySQL" %in% search() || require("RMySQL", 
                                                       quietly = TRUE)) {
    }
    else {
      warning(paste("package:", dQuote("RMySQL"), "cannot be loaded"))
    }
  }
  else {
    stop(paste("package:", dQuote("DBI"), "cannot be loaded."))
  }

  con <- dbConnect(MySQL(), user = user, password = password, 
                   dbname = dbname)
  for (i in 1:length(Symbols)) {
    if (verbose) {
      cat(paste("Loading ", Symbols[[i]], paste(rep(".", 
                                                    10 - nchar(Symbols[[i]])), collapse = ""), sep = ""))
    }
    
    query <- paste("SELECT ", paste(db.fields, collapse = ","), " FROM stockprices where symbol = '",  Symbols[[i]], "' ORDER BY date", sep = "")
    rs <- dbSendQuery(con, query)
    fr <- fetch(rs, n = -1)
    fr <- xts(as.matrix(fr[, -1]), order.by = as.Date(fr[,1], origin = "1970-01-01"), src = dbname, updated = Sys.time())
    colnames(fr) <- paste(Symbols[[i]], c("Open", "High", "Low", "Close", "Volume"), sep = ".")
    fr <- convert.time.series(fr = fr, return.class = return.class)
    if (auto.assign) 
      assign(Symbols[[i]], fr, env)
    if (verbose) 
      cat("done\n")
  }
  dbDisconnect(con)
  if (auto.assign) 
    return(Symbols)
  return(fr)
}

getSymbolNamesMySQL <- function(env = .GlobalEnv, user = NULL, 
                                password = NULL, dbname = NULL, ...) 
{
  return.class = "xts"
  
  this.env <- environment()
  for (var in names(list(...))) {
    assign(var, list(...)[[var]], this.env)
  }
  
  if (require("DBI", quietly = TRUE)) {
    if ("package:RMySQL" %in% search() || require("RMySQL", 
                                                  quietly = TRUE)) {
    }
    else {
      warning(paste("package:", dQuote("RMySQL"), "cannot be loaded"))
    }
  }
  else {
    stop(paste("package:", dQuote("DBI"), "cannot be loaded."))
  }

  con <- dbConnect(MySQL(), user = user, password = password, 
                   dbname = dbname)
  
  query <- paste("SELECT distinct(symbol) from stockprices")
  rs <- dbSendQuery(con, query)
  fr <- fetch(rs, n = -1)

  dbDisconnect(con)
  
  return(fr$symbol)
}

getPositions <- function(env = .GlobalEnv, user = NULL, 
         password = NULL, dbname = NULL, symbol = NULL, ...) 
{
  return.class = "xts"
  
  this.env <- environment()
  for (var in names(list(...))) {
    assign(var, list(...)[[var]], this.env)
  }
  
  if (require("DBI", quietly = TRUE)) {
    if ("package:RMySQL" %in% search() || require("RMySQL", 
                                                  quietly = TRUE)) {
    }
    else {
      warning(paste("package:", dQuote("RMySQL"), "cannot be loaded"))
    }
  }
  else {
    stop(paste("package:", dQuote("DBI"), "cannot be loaded."))
  }
  
  con <- dbConnect(MySQL(), user = user, password = password, 
                   dbname = dbname)
  
  if(is.null(symbol))
    queryStr <- sprintf("SELECT * from positions")
  else
    queryStr <- sprintf("SELECT * from positions where symbol = '%s'", symbol)
  
  query <- paste(queryStr)
  rs <- dbSendQuery(con, query)
  fr <- fetch(rs, n = -1)
  
  dbDisconnect(con)
  
  return(fr)
}

getWallet <- function(env = .GlobalEnv, user = NULL, 
                         password = NULL, dbname = NULL, ...) 
{
  return.class = "xts"
  
  this.env <- environment()
  for (var in names(list(...))) {
    assign(var, list(...)[[var]], this.env)
  }
  
  if (require("DBI", quietly = TRUE)) {
    if ("package:RMySQL" %in% search() || require("RMySQL", 
                                                  quietly = TRUE)) {
    }
    else {
      warning(paste("package:", dQuote("RMySQL"), "cannot be loaded"))
    }
  }
  else {
    stop(paste("package:", dQuote("DBI"), "cannot be loaded."))
  }
  
  con <- dbConnect(MySQL(), user = user, password = password, 
                   dbname = dbname)
  
  queryStr <- sprintf("select distinct(symbol) from positions where end is null")
  
  query <- paste(queryStr)
  rs <- dbSendQuery(con, query)
  fr <- fetch(rs, n = -1)
  
  dbDisconnect(con)
  
  return(fr)
}

getQuery <- function(env = .GlobalEnv, user = NULL, 
                      password = NULL, dbname = NULL, queryStr = "", ...) 
{
  return.class = "xts"
  
  this.env <- environment()
  for (var in names(list(...))) {
    assign(var, list(...)[[var]], this.env)
  }
  
  if (require("DBI", quietly = TRUE)) {
    if ("package:RMySQL" %in% search() || require("RMySQL", 
                                                  quietly = TRUE)) {
    }
    else {
      warning(paste("package:", dQuote("RMySQL"), "cannot be loaded"))
    }
  }
  else {
    stop(paste("package:", dQuote("DBI"), "cannot be loaded."))
  }
  
  con <- dbConnect(MySQL(), user = user, password = password, 
                   dbname = dbname)
  
  #queryStr <- sprintf("select distinct(symbol) from positions where end is null")
  
  query <- paste(queryStr)
  rs <- dbSendQuery(con, query)
  fr <- fetch(rs, n = -1)
  
  dbDisconnect(con)
  
  return(fr)
}

