library('quantmod')
source('mysql_stocks.R')
library('RMySQL')

args_cmd <- commandArgs(trailingOnly=TRUE)                                                                                                                                                                                                                                     

symbols <- c('AEDU3.SA', 'ALLL3.SA', 'AMBV4.SA', 'BBAS3.SA', 'BBDC3.SA', 'BBDC4.SA', 'BBRK3.SA', 'BISA3.SA', 'BOVA11.SA', 'BRAP4.SA', 'BRAX11.SA', 'BRFS3.SA', 'BRKM5.SA', 'BRML3.SA', 'BRPR3.SA', 'BRSR6.SA', 'BTOW3.SA',
             'BVMF3.SA', 'CCRO3.SA', 'CESP6.SA', 'CIEL3.SA', 'CMIG4.SA', 'CPFE3.SA', 'CPLE6.SA', 'CRUZ3.SA', 'CSAN3.SA', 'CSMG3.SA', 'CSMO11.SA', 'CSNA3.SA', 'CTIP3.SA', 'CYRE3.SA', 'DASA3.SA', 'DTEX3.SA', 'ELET3.SA', 'ELET6.SA',
             'ELPL4.SA', 'EMBR3.SA', 'ENBR3.SA', 'EVEN3.SA', 'EZTC3.SA', 'FIBR3.SA', 'FIND11.SA', 'GETI4.SA', 'GFSA3.SA', 'GGBR4.SA', 'GOAU4.SA', 'GOLL4.SA', 'HGTX3.SA', 'HRTP3.SA', 'HYPE3.SA', 'ITSA4.SA', 'ITUB4.SA', 'JBSS3.SA',
             'KLBN4.SA', 'LAME4.SA', 'LIGT3.SA', 'LLXL3.SA', 'LREN3.SA', 'MILA11.SA', 'MMXM3.SA', 'MOBI11.SA', 'MPLU3.SA', 'MPXE3.SA', 'MRFG3.SA', 'MRVE3.SA', 'MULT3.SA', 'MYPK3.SA', 'NATU3.SA', 'ODPV3.SA', 'OGXP3.SA', 'PCAR4.SA',
             'PDGR3.SA', 'PETR3.SA', 'PETR4.SA', 'PIBB11.SA', 'POMO4.SA', 'POSI3.SA', 'PSSA3.SA', 'QGEP3.SA', 'RADL3.SA', 'RAPT4.SA', 'RDCD3.SA', 'RENT3.SA', 'RSID3.SA', 'SANB11.SA', 'SBSP3.SA', 'SMAL11.SA', 'SULA11.SA', 'SUZB5.SA',
             'TBLE3.SA', 'TCSA3.SA', 'TIMP3.SA', 'TOTS3.SA', 'TRPL4.SA', 'UGPA3.SA', 'USIM3.SA', 'USIM5.SA', 'VAGR3.SA', 'VALE3.SA', 'VALE5.SA', 'VIVT4.SA', 'WEGE3.SA')

if(length(args_cmd) == 1)                                                                                                                                                                                                                                                      
{
  symbols <- args_cmd
}

con <- dbConnect(MySQL(), user = "paulo", dbname = "beancounter")

for(i in symbols)
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
}
