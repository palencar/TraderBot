library("xts")
library("quantmod")
library("RSQLite")
library("RMySQL")
library("DBI")
library("config")

getConfig <- memoise(config::get)

#' @export
getSymbolsDaily <- function(Symbols, timeLimit = NULL, adjust = NULL, FilterToday=FALSE, FilterAge=NULL, filterVol = TRUE, env = .GlobalEnv)
{
  if(is.null(Symbols))
    Symbols <- getSymbolNames()

  db.fields = c("date", "day_open", "day_high", "day_low", "day_close", "volume")

  if(FilterToday)
  {
    query <- sprintf("select distinct(symbol) from symbols where symbol in ('%s')", paste(Symbols, collapse = "','"))

    fr <- getQuery(query)

    Symbols <- fr$symbol
  }

  config <- getConfig()

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

    if(!is.null(timeLimit))
      fr <- fr[as.Date(index(fr)) <= as.Date(timeLimit)]

    if(nrow(fr) > 0)
    {
      colnames(fr) <- paste(symbol, c("Open", "High", "Low", "Close", "Volume"), sep = ".")

      if(!is.null(adjust))
        fr <- adjustOHLC.db(fr, adjust = adjust, symbol.name = symbol)

      if(filterVol && is.null(filterVolatility(fr, symbol)))
        next

      assign(symbol, fr, env)
      loaded <- c(loaded, symbol)
    }
  }

  return(loaded)
}

#' @export
getSymbolsIntraday <- function(Symbols, timeFrame = "1H", timeLimit = NULL, adjust = NULL, updateCache = FALSE, filterPeriod = TRUE, filterVol = TRUE, env = .GlobalEnv)
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

      queryStr <- sprintf("select datetime,open,high,low,close,volume from intraday where symbol = '%s' and datetime > '%s'", symbol, lastIdx)

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

    if(!is.null(timeLimit))
      obj <- obj[as.Date(index(obj)) <= as.Date(timeLimit)]

    if(is.null(nrow(obj)) || nrow(obj) == 0)
      next

    if(updateCache & updateFile)
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

    if(filterPeriod)
    {
      if(as.numeric(median(na.omit(diff.xts(index(obj)))), units="mins")
         >
         switch(timeFrame,
                "3M" = 3,
                "5M" = 5,
                "10M" = 10,
                "15M" = 15,
                "30M" = 30,
                "1H" = 60)
         )
      {
        next
      }
    }

    name <- paste0(symbol, ".", timeFrame)

    if(!is.null(adjust))
      obj <- adjustOHLC.db(obj, adjust = adjust, symbol.name = symbol)

    if(format(as.Date(index(xts::last(obj)))) == Sys.Date() &&
       format(Sys.time(), "%H:%M:%S", tz="America/Sao_Paulo") < "17:00:00")
      obj <- head(obj, -1)

    if(filterVol && is.null(filterVolatility(obj, symbol)))
      next

    assign(name, obj, env)

    symbolList <- c(symbolList, name)
  }

  return(symbolList)
}

#' @export
getSymbolNames <- function()
{
  fr <- getQuery("SELECT distinct(symbol) FROM symbols")

  return(fr$symbol)
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

getQuoteDay <- function(SymbolName, Day, modes = 'yahoo')
{
  print(sprintf("getQuoteDay [%s][%s]", SymbolName, Day))

  for(mode in modes)
  {
    if(mode == 'google')
      Symbol <- sprintf("BVMF:%s", unlist(strsplit(SymbolName, "[.]"))[1])

    if(mode == 'yahoo')
      Symbol <- paste0(SymbolName, ".SA")

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
  fr <- getQuery(sprintf("select datetime,close from intraday where symbol = '%s' order by datetime desc limit 1", symbol))
  if(nrow(fr) == 0)
    fr <- getQuery(sprintf("select date as datetime,day_close as close from stockprices where symbol = '%s' order by date desc limit 1", symbol))
  return(fr)
}

loadLocalCSV <- function(symbol)
{
  queryStr <- sprintf("LOAD DATA LOCAL INFILE \'%s.csv\' INTO TABLE stockprices_intraday FIELDS TERMINATED BY \',\' ENCLOSED BY \'\"\' LINES TERMINATED BY \'\n\' (symbol, datetime, min_open, min_low, min_high, min_close, volume)", symbol)
  return(getQuery(queryStr)[,1])
}

getPositions <- function(symbol = NULL, timeFrame = NULL, endDate = NULL, mode = "operation")
{
  fr <- NULL

  if(mode == "operation")
    fr <- getQuery(sprintf("SELECT * from operations where symbol = '%s' order by date", symbol))
  if(mode == "simulation")
  {
    qryStr <- paste0(sprintf("SELECT * from alerts where symbol = '%s'", symbol),
                     ifelse(!is.null(timeFrame), sprintf(" and timeframe = '%s'", timeFrame), ""),
                     ifelse(!is.null(endDate), sprintf(" and datetime <= '%s'", as.character(endDate)), ""),
                     " order by datetime")
    fr <- getQuery(qryStr)

    fr$date <- as.character(fr$datetime)
    fr$type <- fr$alert

    if(nrow(fr) > 0)
    {
      fr$size <- 100
      rl <- rle(fr$type)
      if(rl$values[1] == "sell")
        fr <- tail(fr, nrow(fr)-rl$lengths[1])
    }
  }

  if(is.null(fr) || nrow(fr) == 0)
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
    if(fr[i,]$type == "buy")
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
    else if(fr[i,]$type == "sell")
    {
      if(date != fr[i,]$date || price != fr[i,]$price)
      {
        j <- 1
        if(mode == "simulation")
          vSize <- acSize
        else
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

            if(fr[j,]$size >= vSize)
            {
              acSize <- acSize - fr[j,]$size
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

#' @export
addAlerts <- function(symbol, datetime, alert, price, timeframe)
{
  alerts <- data.frame(symbol, timeframe, datetime, alert, price)

  query <- paste("REPLACE INTO alerts (symbol, timeframe, datetime, alert, price) VALUES",
                 paste(sprintf("('%s', '%s', '%s', '%s', %s)", alerts$symbol, alerts$timeframe, alerts$date, alerts$alert, alerts$price), collapse=', '))

  getQuery(query)
}

#' @export
getAlerts <- function(n = 50, symbols = NULL, types = c("buy", "sell"), openOnly = TRUE)
{
  qryStr <- paste0("select * from alerts",
                   ifelse(is.null(symbols), " ", paste0(" where symbol in ('", paste0(symbols, collapse = "', '"), "') ")),
                   "order by datetime desc",
                   collapse = " ")
  alerts <- data.table(getQuery(qryStr), key=c("symbol","timeframe","alert"))
  if(!is.null(types))
    alerts <- alerts[alerts$alert %in% types,]

  if(openOnly)
  {
    symbDf <- alerts[order(-datetime)][!duplicated(alerts[order(-datetime)][, c("symbol","timeframe")]),  c("symbol","timeframe")]
    alertList <- list()
    for(i in 1:min(n, nrow(symbDf)))
    {
      al <- alerts[symbDf[i, ]][order(-datetime)]
      alertList[[i]] <- head(al, rle(al$alert)$lengths[1])
    }
    return(rbindlist(alertList))
  }

  symbDf <- alerts[order(-datetime)][!duplicated(alerts[order(-datetime)][, c("symbol","timeframe","alert")]), c("symbol","timeframe","alert")]

  return(alerts[head(symbDf[!duplicated(symbDf),], n)])
}

#' @export
getWallet <- function(showClosed = FALSE)
{
  fr <- getQuery("select distinct(symbol) from operations")

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
  config <- getConfig()

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

getOperations <- function(decreasing = FALSE)
{
  if(decreasing)
    getQuery("select * from operations order by date desc")
  else
    getQuery("select * from operations")
}

#' @export
insertOperations <- function(symbol, date, type, size, price, cost)
{
  queryStr <- paste("INSERT INTO operations (symbol, date, type, size, price, cost) VALUES ",
                    sprintf("('%s', '%s', '%s', %d, %f, %f)", symbol, date, type, size, price, cost))
  getQuery(queryStr)
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

  for(symbol in symbols)
  {
    if(mode == 'google')
      name <- sprintf("BVMF:%s", symbol) #Bovespa
    if(mode == 'yahoo')
      name <- sprintf("%s.SA", symbol)   #Bovespa

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

    obj <- na.omit(base::get(name))

    queryStr <- paste("REPLACE INTO stockprices (symbol, date, day_open, day_high, day_low, day_close, volume) VALUES ",
                      paste(sprintf("('%s', '%s', %f, %f, %f, %f, %s)",
                                    symbol, index(obj), as.double(Op(obj)), as.double(Hi(obj)), as.double(Lo(obj)), as.double(Cl(obj)), as.double(Vo(obj))), collapse=', '))
    getQuery(queryStr)

    getQuery(sprintf("REPLACE INTO symbols (symbol) VALUES ('%s')", symbol))
  }
}

#' @export
updateAdjust <- function(symbol, adjust = c("split", "dividend"))
{
  symbol.name <- paste0(symbol, ".SA")

  tryCatch(
  {
    if("dividend" %in% adjust)
    {
      div <- getDividends(symbol.name, from = "1949-01-01")

      lastDiv <- as.Date(getQuery(sprintf("SELECT MAX(date) from dividends where symbol = '%s'", symbol))[,1])

      if(!is.na(lastDiv) && is.xts(div))
        div <- div[index(div) > lastDiv]

      if(is.xts(div) && nrow(div) > 0)
      {
        queryStr <- paste("INSERT INTO dividends (symbol, date, dividend) VALUES ",
                          paste(sprintf("('%s', '%s', %f)", symbol, index(div), as.numeric(div)), collapse = ", "))
        getQuery(queryStr)
      }
    }

    if("split" %in% adjust)
    {
      splits <- getSplits(symbol.name, from = "1949-01-01")

      lastSplit <- as.Date(getQuery(sprintf("SELECT MAX(date) from splits where symbol = '%s'", symbol))[,1])

      if(!is.na(lastSplit) && is.xts(splits))
        splits <- splits[index(splits) > lastSplit]

      if(is.xts(splits) && nrow(splits) > 0)
      {
        queryStr <- paste("INSERT INTO splits (symbol, date, split) VALUES ",
                          paste(sprintf("('%s', '%s', %f)", symbol, index(splits), as.numeric(splits)), collapse = ", "))
        getQuery(queryStr)
      }
    }
  },
  error = function(err)
  {
    print(symbol)
    print(err)
  })
}

#' @export
getDividends.db <- memoise(function(symbol)
{
  df <- getQuery(sprintf("select * from dividends where symbol = '%s'", symbol))
  xts(df$dividend, order.by = as.Date(df$date, origin = "1970-01-01"))
}, ~timeout(600))

#' @export
getSplits.db <- memoise(function(symbol)
{
  df <- getQuery(sprintf("select * from splits where symbol = '%s'", symbol))
  xts(df$split, order.by = as.Date(df$date, origin = "1970-01-01"))
}, ~timeout(600))

#' @export
saveSymbolsIntraday <- function(symbols)
{
  for(symbol in symbols)
  {
    df <- f.get.google.intraday(symbol, 60, "15d")

    if(!is.null(df))
      insertIntraday(symbol, df)

    getQuery(sprintf("REPLACE INTO symbols (symbol) VALUES ('%s')", symbol))
  }
}

#' @export
updateDaily <- function(symbolNames = getSymbolNames())
{
  symbolList <- NULL
  quotes = getQuote(paste(symbolNames, "SA", sep = "."))

  for(i in 1:length(symbolNames))
  {
    if(anyNA(as.numeric(quotes[i,])) || quotes[i,"Volume"] == 0)
    {
      warning(paste0("Bad Data: ", symbolNames[i], " ", as.Date(quotes[i, c("Trade Time")]), " ", paste(quotes[i, c("Open", "High", "Low", "Last", "Volume")], collapse = " ")))
      next
    }

    queryStr <- sprintf("REPLACE INTO stockprices (symbol, date, day_open, day_high, day_low, day_close, volume) VALUES('%s', '%s', %s, %s, %s, %s, %s)",
                        symbolNames[i], as.Date(quotes[i, "Trade Time"]), quotes[i, "Open"], quotes[i, "High"], quotes[i, "Low"], quotes[i, "Last"], quotes[i, "Volume"])

    getQuery(queryStr)

    symbolList <- c(symbolList, symbolNames[i])
  }

  return(symbolList)
}

#' @export
updateDailyFromIntraday <- function(symbols = getSymbolNames(), tradeDates = Sys.Date())
{
  symbolList <- NULL
  env = new.env()

  for(symbol in symbols)
  {
    symbol1M = getSymbolsIntraday(symbol, "1M", filterPeriod = FALSE, filterVol = FALSE, env = env)

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
                      symbol, index(obj), as.double(Op(obj)), as.double(Hi(obj)), as.double(Lo(obj)), as.double(Cl(obj)), as.double(Vo(obj))), collapse=', '))

    getQuery(queryStr)

    base::rm(list = symbol1M, envir = env)

    symbolList <- c(symbolList, symbol)
  }

  return(symbolList)
}

#' @export
updateIntraday <- function(symbols = NULL)
{
  symbolList <- NULL

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

    mins <- min(round(as.numeric(difftime(Sys.time(), lastPrice(symbol)$datetime, units = "mins"))), 500)
    df <- tryCatch(uolIntraday(symbol, mins),
                   error = function(err)
                   {
                     print(err)
                     return(NULL)
                   })

    if(any(df$Volume == 0))
      warning(paste("Zero volume:", symbol, "[", index(df[df$Volume == 0, ]), "]", collapse=" "))

    if(!is.null(df) && any(last(index(df)) > lastPrice(symbol)$datetime))
    {
      insertIntraday(symbol, df, lastIdx)
      symbolList <- c(symbolList, symbol)
    }
  }

  return(unlist(symbolList))
}

getAssets <- memoise(function()
{
  base = 'http://cotacoes.economia.uol.com.br/ws/asset/stock/list?size=10000'
  assets <- content(GET(base),type="text")
  assets <- fromJSON(assets)
  df <- rbindlist(assets$data)

  df[df$code %in% paste0(getSymbolNames(), ".SA"), c('code', 'idt')]
})

uolIntraday <- function(symbol, mins = 500)
{
  assets <- getAssets()
  idt <- assets[assets$code == paste0(symbol, ".SA"), ]$idt
  url <- paste0("http://cotacoes.economia.uol.com.br/ws/asset/", idt, "/intraday?size=", mins, "&callback=uolfinancecallback0&fields=date,price,open,low,high,vol")
  text <- content(GET(url), type="text", encoding = "UTF-8")
  text <- sub("uolfinancecallback0\\(", "", text)
  text <- sub("\\);$","",text)
  json <- fromJSON(text)

  df <- rbindlist(json$data)
  if(nrow(df) == 0)
    return(NULL)

  df$datetime <- as.POSIXct(df$date/1000, origin = "1970-01-01", tz="America/Sao_Paulo")
  df <- df[order(df$datetime), c('datetime', 'price', 'vol')]
  df[, c('Open', 'High', 'Low', 'Close')] <- df$price

  if(nrow(df) > 1)
    df$Volume <- apply(lag(zoo(df$vol), c(-1,0), na.pad = TRUE), 1L, diff)
  else
    df$Volume <- df$vol

  df <- na.omit(df[df$Volume > 0])

  xts(df[, c('Open', 'High', 'Low', 'Close', 'Volume')], order.by = df$datetime)
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
    na.omit(read.csv(full.url, header = FALSE, skip = 7, stringsAsFactors = FALSE))
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
