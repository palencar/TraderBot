library("xts")
library("quantmod")
library("RSQLite")
library("DBI")


getSymbolsDB <- function (Symbols, FilterToday=FALSE, FilterAge=NULL, env = .GlobalEnv)
{
  db.fields = c("date", "day_open", "day_high", "day_low", "day_close", "volume")

  if(FilterToday)
  {
    query <- sprintf("select distinct(symbol) from stockprices where symbol in ('%s')", paste(Symbols, collapse = "','"))

    fr <- getQuery(query)

    Symbols <- fr$symbol
  }

  if(!is.null(FilterAge))
  {
    query <- sprintf("select * from (select symbol, (julianday(max(date))-julianday(min(date))) as days from stockprices group by symbol) as age where days > %d", FilterAge)

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

      queryStr <- sprintf("REPLACE INTO stockprices (symbol, date, day_open, day_high, day_low, day_close, volume) VALUES('%s', '%s', %f, %f, %f, %f, %g)",
                          symbolNames[i], as.Date(quotes[i, 1]), as.double(table[1,1]), as.double(table[1,2]), as.double(table[1,3]), as.double(table[1,4]),
                          as.double(table[1,5]))

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

    symbol <- get(i)
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

    if(is.na(Op(get(Symbol))) || is.na(Hi(get(Symbol))) ||
       is.na(Lo(get(Symbol))) || is.na(Cl(get(Symbol))) ||
       as.double(Op(get(Symbol))) == 0.0 || as.double(Hi(get(Symbol))) == 0.0 ||
       as.double(Lo(get(Symbol))) == 0.0 || as.double(Cl(get(Symbol))) == 0.0 )
      return(NULL)

    table <- as.data.frame(get(Symbol))
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

    print(get(Symbol))

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
  return(getQuery("select max(date) from stockprices")[,1])
}

lastPrice <- function(SymbolName)
{
  return(getQuery(sprintf("select day_close from stockprices where symbol = '%s' order by date desc limit 1", SymbolName)))
}

lastTradeDay <- function(SymbolName)
{
  objName <- paste("lastTradeDay", SymbolName, sep = "")
  if(exists(objName))
    return(get(objName))

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
  if(exists("AllTradeDays"))
    return(get("AllTradeDays"))

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
  dbConn <- dbConnect(RSQLite::SQLite(), "db.sqlite")
  fr <- dbGetQuery(dbConn, queryStr)
  dbDisconnect(dbConn)

  return(fr)
}

