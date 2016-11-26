library('quantmod')
source('R/dbInterface.R')

#' @export
saveSymbols <- function(symbols)
{
  invisible(Sys.setlocale("LC_MESSAGES", "C"))
  invisible(Sys.setlocale("LC_TIME", "C"))

  mode = 'google'

  symbols <- sub("^([^.]*).*", "\\1", symbols)

  con <- dbConnect(RSQLite::SQLite(), "db.sqlite")
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

    table <- as.data.frame(get(name))
    dates <- index(get(name))
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
    Sys.sleep(5)
  }

  dbDisconnect(con)
}
