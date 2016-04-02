library('quantmod')
library('RMySQL')
source('dbInterface.R')

args_cmd <- commandArgs(trailingOnly=TRUE)                                                                                                                                                                                                   

#symbols <- getSymbolNamesMySQL() 

con <- dbConnect(MySQL(), default.file='mysql.config', db="beancounter")

mode = 'yahoo'

if(length(args_cmd) < 2)
{
  print("Use [google|yahoo] symbol")
  quit()
}

mode <- args_cmd[1]
symbols <- tail(args, n=(length(args)-1))

for(i in symbols)
{
  if(mode == 'google')
    i <- sprintf("BVMF:%s", gsub(".SA", "", i))
  
  print(i)
  getSymbols(i, src=mode, from=as.Date('2010-01-01'), to=as.Date(Sys.Date()))
  table <- as.data.frame(get(i))
  names(table)[1]<-paste("day_open")
  names(table)[2]<-paste("day_high")
  names(table)[3]<-paste("day_low")
  names(table)[4]<-paste("day_close")
  names(table)[5]<-paste("volume")
  table["date"] <- as.Date(index(get(i)))
  table["symbol"] <- i
  table[6] <- NULL
  
  #dbWriteTable(con, name = "stockprices", table, append = T, row.names = F)
  #Sys.sleep(10)
  print(get(i))
}

dbDisconnect(con)

