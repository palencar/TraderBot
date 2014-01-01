source("startProbe.R")
source("filters.R")
source("polyReg.R")
source("chart.R")
library('quantmod')
source('mysql_stocks.R')
library('RMySQL')
                                                                                                                                                             

args <- commandArgs(trailingOnly=TRUE)
print(args)

Symbols <- startProbe()

startDate <- as.Date(Sys.Date() - 1500)
endDate <- Sys.Date()

if(length(args) >= 1)
{
  Symbols <- args
}
     
con <- dbConnect(MySQL(), user = "paulo", dbname = "beancounter")

for(i in Symbols)
{
  print(i)
  getSymbols(i, src="yahoo")
  table <- as.data.frame(get(i))
  names(table)[1]<-paste("day_open")
  names(table)[2]<-paste("day_high")
  names(table)[3]<-paste("day_low")
  names(table)[4]<-paste("day_close")
  names(table)[5]<-paste("volume")
  table["date"] <- as.Date(index(get(i)))
  table["symbol"] <- i
  table[6] <- NULL
  
  dbWriteTable(con, name = "stockprices", table, append = T, row.names = F)

  Sys.sleep(2)
}
   
computeRegressions(Symbols, startDate, endDate)

alertSymbols <- filterObjectsSets(Symbols, startDate, endDate)

chartSymbols(alertSymbols)
    
#if(length(alertSymbols) > 0)
#{
#   for(i in alertSymbols)
#     imgAttachmets <- sprintf("-a charts/%s.png", alertSymbols)
#      
#   muttCmd <- sprintf("echo \"%s\" | mutt -s \"Trader Bot Alert\" pbalencar@yahoo.com %s", sprintf("Snapshot time: %s", startTime), paste(imgAttachmets, collapse=" "))
#      
#   cmdOut <- system(muttCmd, intern=TRUE, ignore.stderr=TRUE)
#}
