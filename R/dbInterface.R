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
getSymbolsIntraday <- function(Symbols, timeFrame = "5M", updateLast = FALSE, filterPeriod = TRUE, env = .GlobalEnv)
{
  symbolList <- NULL

  if(is.null(Symbols))
  {
    Symbols <- getSymbolNames()
  }

  for(symbol in Symbols)
  {
    lastIdx <- NULL
    updateFile <- FALSE

    name1M <- paste0("datacache/", symbol, ".1M.rds")

    if(file.exists(name1M))
    {
      obj <- readRDS(name1M)

      lastIdx <- index(tail(obj, 1))
      lastIdx <- format(lastIdx, usetz = FALSE)

      cmp <- ifelse(updateLast, ">=", ">")
      queryStr <- sprintf("select datetime,open,high,low,close,volume from intraday where symbol = '%s' and datetime %s '%s'", symbol, cmp, lastIdx)

      fr <- getQuery(queryStr)
      objQry <- xts(fr[,-1], as.POSIXct(strptime(fr[,1], '%Y-%m-%d %H:%M:%S'), tz='America/Sao_Paulo'))

      if(nrow(objQry) > 0)
      {
        obj <- rbind(obj, objQry)
        updateFile <- TRUE
      }
    }
    else
    {
      queryStr <- sprintf("select datetime,open,high,low,close,volume from intraday where symbol = '%s'", symbol)

      fr <- getQuery(queryStr)
      obj <- xts(fr[,-1], as.POSIXct(strptime(fr[,1], '%Y-%m-%d %H:%M:%S'), tz='America/Sao_Paulo'))
      updateFile <- TRUE
    }

    if(is.null(nrow(obj)) || nrow(obj) == 0)
      next

    obj <- obj[!duplicated(index(obj))]

    if(updateFile)
      saveRDS(obj, name1M)

    if(nrow(obj) <= 1)
      next

    if(filterPeriod)
    {
      pr <- periodicity(obj)
      if(pr$frequency > switch(timeFrame, "1M" = 1, "3M" = 3, "5M" = 5, "10M" = 10, "15M" = 15, "30M" = 30, "1H" = 60))
      {
        print(paste0("Periodicity [", symbol, "]: ", pr$frequency, " > ", timeFrame))
        next
      }
    }

    if(timeFrame != "1M")
    {
      obj <- switch(timeFrame,
                  "3M" = to.minutes3(obj),
                  "5M" = to.minutes5(obj),
                  "10M" = to.minutes10(obj),
                  "15M" = to.minutes15(obj),
                  "30M" = to.minutes30(obj),
                  "1H" = to.hourly(obj))

      obj <- align.time(obj)
    }

    name <- paste0(symbol, ".", timeFrame)

    assign(name, obj, env)

    symbolList <- c(symbolList, name)
  }

  return(symbolList)
}

#' @export
getSymbolNames <- function()
{
  fr <- getQuery("SELECT distinct(symbol) FROM stockprices UNION SELECT distinct(symbol) FROM intraday")

  return(fr$symbol)
}

#' @export
getSymbolsDaily <- function(symbolNames = NULL, minAge = NULL)
{
  if(is.null(symbolNames))
    symbolNames <- getSymbolNames()

  if(is.null(symbolNames) || length(symbolNames) == 0)
    return(NULL)

  symbolNamesObj <- getSymbolsDB(symbolNames, FilterAge=minAge)

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
    names(table) <- c("day_open", "day_high", "day_low", "day_close", "volume")
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

lastPrice <- function(symbol)
{
  queryStr <- sprintf("select close from intraday where symbol = '%s' order by datetime desc limit 1", symbol)

  return(as.numeric(getQuery(queryStr)))
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

  return(positions)
}

getWallet <- function(showClosed = FALSE)
{
  fr <- getQuery("select distinct(symbol) from operations") # where closeVal is null

  symbols <- c()

  if(showClosed == TRUE)
  {
    return(fr$symbol)
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

  if(is.null(config$engine))
    return(NULL)

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

insertIntraday <- function(symbol, data, from = NULL)
{
  if(nrow(data) == 0)
    return(NULL)

  df <- data[paste0(from, "/")]

  if(nrow(df) == 0)
    return(NULL)

  queryStr <- paste("REPLACE INTO intraday (symbol, datetime, open, high, low, close, volume) VALUES ",
                      paste(sprintf("('%s', '%s', %f, %f, %f, %f, %g)", symbol, format(index(df), "%Y-%m-%d %H:%M:%S"),
                                    df$Open, df$High, df$Low, df$Close, df$Volume), collapse=', '))

  getQuery(queryStr)
}

#' @export
saveSymbolsDaily <- function(symbols, mode = 'google')
{
  invisible(Sys.setlocale("LC_MESSAGES", "C"))
  invisible(Sys.setlocale("LC_TIME", "C"))

  symbols <- sub("^([^.]*).*", "\\1", symbols)

  print(symbols)

  for(i in symbols)
  {
    if(mode == 'google')
      name <- sprintf("BVMF:%s", i) #Bovespa
    if(mode == 'yahoo')
      name <- sprintf("%s.SA", i)   #Bovespa

    print(name)

    state <- tryCatch({
      getSymbols(name, src=mode)
      T
    }, warning = function(war) {
      print(war)
      F
    }, error = function(err) {
      print(err)
      F
    }, finally={
    })

    if(state == F)
      next

    table <- as.data.frame(base::get(name))
    dates <- index(base::get(name))
    if(mode == 'google')
    {
      name <- sprintf("BVMF:%s", i)
    }
    if(mode == 'yahoo')
    {
      name <- sprintf("%s.SA", i)
    }

    for(j in index(table))
    {
      row <- table[j,]
      names(row)[1]<-paste("day_open")
      names(row)[2]<-paste("day_high")
      names(row)[3]<-paste("day_low")
      names(row)[4]<-paste("day_close")
      names(row)[5]<-paste("volume")
      row["date"] <- dates[j]
      row["symbol"] <- name
      row[6] <- NULL

      if(is.na(row[1]) || is.na(row[2]) || is.na(row[3]) || is.na(row[4]) || is.na(row[5]))
      {
        warning(print(paste(row)))
        next
      }

      queryStr <- sprintf("REPLACE INTO stockprices (symbol, date, day_open, day_high, day_low, day_close, volume) VALUES('%s', '%s', %f, %f, %f, %f, %g)",
                          i, dates[j], as.double(row[1]), as.double(row[2]), as.double(row[3]), as.double(row[4]),
                          as.double(row[5]))
      getQuery(queryStr)
    }
  }
}

#' @export
saveSymbolsIntraday <- function(symbols)
{
  for(symbol in symbols)
  {
    df <- f.get.google.intraday(symbol, 60, "15d")

    if(!is.null(df))
      insertIntraday(symbol, df)
  }
}

#' @export
updateDaily <- function(symbolNames = getSymbolNames())
{
  quotes = getQuote(paste(symbolNames, "SA", sep = "."), what = yahooQuote.EOD)

  for(i in 1:length(symbolNames))
  {
    if(anyNA(as.numeric(quotes[i,])) || quotes[i,"Volume"] == 0)
    {
      warning(paste0("Bad Data: ", symbolNames[i], " ", as.Date(quotes[i, c("Trade Time")]), " ", paste(quotes[i, c("Open", "High", "Low", "Close", "Volume")], collapse = " ")))
      next
    }

    queryStr <- sprintf("REPLACE INTO stockprices (symbol, date, day_open, day_high, day_low, day_close, volume) VALUES('%s', '%s', %s, %s, %s, %s, %s)",
                        symbolNames[i], as.Date(quotes[i, "Trade Time"]), quotes[i, "Open"], quotes[i, "High"], quotes[i, "Low"], quotes[i, "Close"], quotes[i, "Volume"])

    getQuery(queryStr)
  }
}

#' @export
updateDailyFromIntraday <- function(symbols = getSymbolNames(), tradeDates = Sys.Date())
{
  env = new.env()

  for(symbol in symbols)
  {
    symbol1M = getSymbolsIntraday(symbol, "1M", filterPeriod = FALSE, env = env)

    if(is.null(symbol1M) || !exists(symbol1M, envir = env))
      next

    obj <- base::get(symbol1M, envir = env)
    obj <- align.time(to.daily(obj)[paste0(min(tradeDates), "/", max(tradeDates))])

    if(nrow(obj) == 0)
      next

    names(obj) <- c("day_open", "day_high", "day_low", "day_close", "volume")

    if(anyNA(as.numeric(obj)))
    {
      warning(paste0("Bad Data: ", symbol1M, " ", paste(obj, collapse = " ")))
      next
    }

    if(any(obj$volume == 0))
      warning(paste("Zero volume:", symbol, "[", index(obj[obj$volume == 0, ]), "]", collapse=" "))

    queryStr <- paste("REPLACE INTO stockprices (symbol, date, day_open, day_high, day_low, day_close, volume) VALUES ",
                      paste(sprintf("('%s', '%s', %f, %f, %f, %f, %s)",
                      symbol, index(obj), as.double(obj$day_open), as.double(obj$day_high), as.double(obj$day_low), as.double(obj$day_close), as.double(obj$volume)), collapse=', '))

    getQuery(queryStr)

    base::rm(list = symbol1M, envir = env)
  }
}

#' @export
updateIntraday <- function(symbols = NULL)
{
  if(is.null(symbols))
  {
    symbols <- getSymbolNames()
  }

  for(symbol in symbols)
  {
    print(symbol)

    lastIdx <- NULL

    name1M <- paste0("datacache/", symbol, ".1M.rds")

    if(file.exists(name1M))
    {
      obj <- readRDS(name1M)

      lastIdx <- index(tail(obj, 1))
      lastIdx <- format(lastIdx, usetz = FALSE)
    }

    df <- f.get.google.intraday(symbol, 60, "1d")

    if(any(df$Volume == 0))
      warning(paste("Zero volume:", symbol, "[", index(df[df$Volume == 0, ]), "]", collapse=" "))

    if(!is.null(df))
      insertIntraday(symbol, df, lastIdx)
  }
}

#
# Adapted from
# https://github.com/frederickpelchat/quantitative-finance/blob/master/intraday-data.R
#
f.get.google.intraday <- function(symbol, freq, period) {
  base.url <- 'http://finance.google.com/finance/getprices?'
  options.url <- paste('i=', freq, '&p=', period, '&f=d,o,h,l,c,v&df=cpct&q=', symbol, sep = '')
  full.url <- paste(base.url, options.url, sep = '')

  data <- tryCatch({
    read.csv(full.url, header = FALSE, skip = 7, stringsAsFactors = FALSE)
  }, error = function(err)
  {
    #print(err)
    return(NULL)
  })

  if(is.null(data))
    return(data.frame())

  starting.times.idx <- which(substring(data$V1, 1, 1) == 'a')
  ending.seconds.idx <- c(starting.times.idx[-1] - 1, nrow(data))
  r.str.idx.use <- paste(starting.times.idx, ':', ending.seconds.idx, sep = '')

  starting.times <- as.numeric(substring(data[starting.times.idx, 1], 2))

  data[starting.times.idx, 1] <- 0
  clean.idx <- do.call(c, lapply(seq(1, length(r.str.idx.use)),
                                 function(i) {
                                   starting.times[i] + freq * as.numeric(data[eval(parse(text = r.str.idx.use[i])), 1])
                                 })
  )
  data.xts <- xts(data[,-1], as.POSIXct(clean.idx, origin = '1970-01-01', tz = 'America/Sao_Paulo'))

  indexTZ(data.xts) <- 'America/Sao_Paulo'
  colnames(data.xts) <- c('Open', 'High', 'Low', 'Close', 'Volume')

  data.xts
}
