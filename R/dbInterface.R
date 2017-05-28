library("xts")
library("quantmod")
library("RSQLite")
library("RMySQL")
library("DBI")
library("config")

getSymbolsDB <- function (Symbols, FilterToday=FALSE, FilterAge=NULL, env = .GlobalEnv)
{
  db.fields = c("date", "day_open", "day_high", "day_low", "day_close", "volume")

  if(FilterToday)
  {
    query <- sprintf("select distinct(symbol) from stockprices where symbol in ('%s')", paste(Symbols, collapse = "','"))

    fr <- getQuery(query)

    Symbols <- fr$symbol
  }

  config <- config::get()

  if(!is.null(FilterAge))
  {
    if(config$engine == "sqlite")
    {
      query <- sprintf("select * from (select symbol, (julianday(max(date))-julianday(min(date))) as days from stockprices group by symbol) as age where days > %d", FilterAge)
    }
    if(config$engine == "mysql")
    {
      query <- sprintf("select * from (select symbol, DATEDIFF(max(date), min(date)) as days from stockprices group by symbol) as age where days > %d", FilterAge)
    }

    fr <- getQuery(query)
    Symbols <- intersect(Symbols, fr$symbol)
  }

  loaded <- NULL

  for (symbol in Symbols)
  {
    query <- paste("SELECT ", paste(db.fields, collapse = ","), " FROM stockprices where symbol = '",  symbol, "' ORDER BY date", sep = "")

    fr <- getQuery(query)
    fr <- xts(as.matrix(fr[, -1]), order.by = as.Date(fr[,1], origin = "1970-01-01"), updated = Sys.time())
    if(nrow(fr) > 0)
    {
      colnames(fr) <- paste(symbol, c("Open", "High", "Low", "Close", "Volume"), sep = ".")
      assign(symbol, fr, env)
      loaded <- c(loaded, symbol)
    }
  }

  return(loaded)
}

#' @export
getSymbolsIntraday <- function(Symbols, timeFrame = "5M")
{
  symbolList <- NULL

  for(symbol in Symbols)
  {
    fr <- getQuery(sprintf("select datetime,open,high,low,close,volume from intraday where symbol = '%s'", symbol))
    obj <- xts(fr[,-1], as.POSIXct(as.POSIXct(strptime(fr[,1], '%Y-%m-%d %H:%M:%S'))))

    if(nrow(obj) == 0)
      next

    obj <- switch(timeFrame,
                  "3M" = to.minutes3(obj),
                  "5M" = to.minutes5(obj),
                  "10M" = to.minutes10(obj),
                  "15M" = to.minutes15(obj),
                  "30M" = to.minutes30(obj),
                  "1H" = to.hourly(obj))

    obj <- align.time(obj)

    name <- paste0(symbol,".", timeFrame)

    assign(name, obj, .GlobalEnv)

    symbolList <- c(symbolList, name)
  }

  return(symbolList)
}

#' @export
getSymbolNames <- function()
{
  fr <- getQuery("SELECT distinct(symbol) from stockprices")

  return(fr$symbol)
}

#' @export
startProbe <- function(symbolNames = NULL, update = TRUE, minAge = NULL)
{
  if(is.null(symbolNames))
    symbolNames <- getSymbolNames()

  if(is.null(symbolNames) || length(symbolNames) == 0)
    return(NULL)

  if(update)
  {
    quotes = getQuote(paste(symbolNames, "SA", sep = "."), what = yahooQuote.EOD)

    for(i in 1:length(symbolNames))
    {
      if(is.na(quotes[i, 1]))
        next

      table <- coredata(xts(quotes[i, -1], as.Date(quotes[i, 1])))

      if(table[1] == "N/A" || table[2] == "N/A" || table[3] == "N/A" || table[4] == "N/A" || table[5] == "N/A" ||
         table[1] == 0.0   || table[2] == 0.0   || table[3] == 0.0   || table[4] == 0.0)
      {
        warning(sprintf("NA value in %s [%s %s %s %s %s]", symbolNames[i], table[1], table[2], table[3], table[4], table[5]))
        next
      }

      queryStr <- sprintf("REPLACE INTO stockprices (symbol, date, day_open, day_high, day_low, day_close, volume) VALUES('%s', '%s', %f, %f, %f, %f, %d)",
                          symbolNames[i], as.Date(quotes[i, 1]), as.double(table[1,1]), as.double(table[1,2]), as.double(table[1,3]), as.double(table[1,4]),
                          as.numeric(table[1,5]))

      getQuery(queryStr)
    }
  }

  symbolNamesObj <- getSymbolsDB(symbolNames, FilterToday=update, FilterAge=minAge)

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

    symbol <- base::get(i)
    symbol[date,1] <- quote$Open
    symbol[date,2] <- quote$High
    symbol[date,3] <- quote$Low
    symbol[date,4] <- quote$Last
    assign(i, symbol)
  }
}

getQuoteDay <- function(SymbolName, Day)
{
  print(sprintf("getQuoteDay [%s][%s]", SymbolName, Day))

  modes <- c('google', 'yahoo')

  for(mode in modes)
  {
    if(mode == 'google')
      Symbol <- sprintf("BVMF:%s", unlist(strsplit(SymbolName, "[.]"))[1])

    try(getSymbols(Symbol, src=mode, from=as.Date(Day), to=as.Date(Day)), silent=TRUE)
    if(exists(Symbol) == FALSE)
      return(NULL)

    if(is.na(Op(base::get(Symbol))) || is.na(Hi(base::get(Symbol))) ||
       is.na(Lo(base::get(Symbol))) || is.na(Cl(base::get(Symbol))) ||
       as.double(Op(base::get(Symbol))) == 0.0 || as.double(Hi(base::get(Symbol))) == 0.0 ||
       as.double(Lo(base::get(Symbol))) == 0.0 || as.double(Cl(base::get(Symbol))) == 0.0 )
      return(NULL)

    table <- as.data.frame(base::get(Symbol))
    names(table)[1]<-paste("day_open")
    names(table)[2]<-paste("day_high")
    names(table)[3]<-paste("day_low")
    names(table)[4]<-paste("day_close")
    names(table)[5]<-paste("volume")
    table["date"] <- as.Date(Day)
    table["symbol"] <- SymbolName
    table[6] <- NULL

    queryStr <- sprintf("REPLACE INTO stockprices (symbol, date, day_open, day_low, day_high, day_close, volume) VALUES('%s', '%s', %f, %f, %f, %f, %g)",
                        SymbolName, as.Date(Day), table[1,1], table[1,2], table[1,3], table[1,4], table[1,5])

    print(base::get(Symbol))

    getQuery(queryStr)
  }
}

meanPrice <- function(SymbolName)
{
  positions <- getPositions(SymbolName)
  opValue <- 0
  opSize <- 0

  for(pos in positions)
  {
    if(is.na(pos$end))
    {
      opValue <- opValue + (pos$openVal * pos$size)
      opSize <- opSize + pos$size
    }
  }

  if(opSize > 0)
    return(opValue/opSize)

  return(NULL)
}

lastTradingSession <- function()
{
  return(as.Date(getQuery("select max(date) from stockprices")[,1]))
}

lastPrice <- function(SymbolName, dateLimit = NULL)
{
  if(is.null(dateLimit))
    queryStr <- sprintf("select day_close from stockprices where symbol = '%s' order by date desc limit 1", SymbolName)
  else
    queryStr <- sprintf("select day_close from stockprices where symbol = '%s' and date <= date('%s') order by date desc limit 1", SymbolName, dateLimit)

  return(getQuery(queryStr))
}

lastTradeDay <- function(SymbolName)
{
  objName <- paste("lastTradeDay", SymbolName, sep = "")
  if(exists(objName))
    return(base::get(objName))

  day <- getQuery(sprintf("select max(date) from stockprices where symbol = '%s'", SymbolName))

  assign(objName, day, .GlobalEnv)

  return(day)
}

loadLocalCSV <- function(symbol)
{
  queryStr <- sprintf("LOAD DATA LOCAL INFILE \'%s.csv\' INTO TABLE stockprices_intraday FIELDS TERMINATED BY \',\' ENCLOSED BY \'\"\' LINES TERMINATED BY \'\n\' (symbol, datetime, min_open, min_low, min_high, min_close, volume)", symbol)
  return(getQuery(queryStr)[,1])
}

getPositions <- function(symbol = NULL)
{
  queryStr <- sprintf("SELECT * from operations where symbol = '%s' order by date", symbol)

  fr <- getQuery(queryStr)
  if(nrow(fr) == 0)
  {
    return(NULL)
  }

  acValue <- 0
  acSize <- 0
  n <- 0
  positions <- c()

  date <- ''
  price <- ''

  i <- 1
  while(i <= nrow(fr))
  {
    if(fr[i,]$type == 'C')
    {
      acSize <- acSize + fr[i,]$size

      if(date != fr[i,]$date || price != fr[i,]$price)
      {
        n <- n + 1
        position <- c()
        position$start <- fr[i,]$date
        position$openVal <- fr[i,]$price
        position$size <- fr[i,]$size
        position$end <- NA
        position$closeVal <- NA
        positions[[n]] <- position

        date <- fr[i,]$date
        price <- fr[i,]$price
      }
    }
    else if(fr[i,]$type == 'V')
    {
      if(date != fr[i,]$date || price != fr[i,]$price)
      {
        j <- 1
        vSize <- fr[i,]$size
        while(j <= length(positions) && j < i && acSize > 0)
        {
          position <- positions[[j]]
          if(is.na(position$closeVal))
          {
            position$end <- fr[i,]$date
            position$closeVal <- fr[i,]$price
            positions[[j]] <- position
            date <- fr[i,]$date
            price <- fr[i,]$price

            #TODO validar acSize >= 0

            if(fr[j,]$size >= vSize)
            {
              acSize <- acSize - fr[i,]$size
              break
            }
            else
            {
              acSize <- acSize - fr[j,]$size
              vSize <- vSize - fr[j,]$size
              j <- j + 1
            }
          }
          else
          {
            j <- j + 1
          }
        }
      }
    }
    i <- i + 1
  }

  #print(positions)

  return(positions)
}

getWallet <- function(FilterClosed = TRUE)
{
  fr <- getQuery("select distinct(symbol) from operations") # where closeVal is null

  symbols <- c()

  if(FilterClosed == FALSE)
  {
    for(i in fr[[1]])
    {
      symbols <- c(symbols, i)
    }

    return(symbols)
  }


  for(i in fr[[1]])
  {
    positions <- getPositions(i)
    opSize <- 0

    for(pos in positions)
    {
      if(is.na(pos$end))
      {
        symbols <- c(symbols, i)
      }
    }
  }

  return(unique(symbols))
}

getTradeDays <- function(symbols = NULL)
{
  if(is.null(symbols))
    queryStr <- sprintf("select distinct date from stockprices order by date asc")
  else
    queryStr <- sprintf("select distinct date from stockprices where symbol in ('%s') order by date asc", paste(symbols, collapse = "','"))

  paste("'", paste(symbols, collapse = "','"), sep="", ",")
  allTradeDays <- getQuery(queryStr)[,1]

  assign("AllTradeDays", allTradeDays, .GlobalEnv)

  return(allTradeDays)
}

getQuery <- function(queryStr = "")
{
  config <- config::get()

  if(config$engine == "mysql")
  {
    dbConn <- dbConnect(RMySQL::MySQL(), user=config$user, password=config$password, dbname=config$database, host=config$host)
  }

  if(config$engine == "sqlite")
  {
    dbConn <- dbConnect(RSQLite::SQLite(), config$database)
    dbGetQuery(dbConn, "PRAGMA busy_timeout=5000;")
  }

  fr <- dbGetQuery(dbConn, queryStr)
  dbDisconnect(dbConn)

  return(fr)
}

insertIntraday <- function(name)
{
  csv <- read.csv(name, header = F, col.names = c("symbol", "date", "time", "open", "high", "low", "close", "volume"))
  dff <- within(data.frame(csv), { datetime=format(as.POSIXct(paste(date, time)), "%Y-%m-%d %H:%M:%S") })

  if(nrow(dff) > 0)
  for(i in 1:nrow(dff))
  {
    df <- dff[i,]
    if(config$engine == "sqlite")
      queryStr <- sprintf("INSERT OR IGNORE INTO intraday (symbol, datetime, open, high, low, close, volume) VALUES('%s', '%s', %f, %f, %f, %f, %g)",
                        df$symbol, df$datetime, df$open, df$high, df$low, df$close, df$volume)
    if(config$engine == "mysql")
      queryStr <- sprintf("INSERT IGNORE INTO intraday (symbol, datetime, open, high, low, close, volume) VALUES('%s', '%s', %f, %f, %f, %f, %g)",
                          df$symbol, df$datetime, df$open, df$high, df$low, df$close, df$volume)
    getQuery(queryStr)
  }
}
