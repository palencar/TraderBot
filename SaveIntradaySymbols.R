library('quantmod')
source('dbInterface.R')
library('RMySQL')

symbols <- c('AEDU3', 'ALLL3', 'AMBV4', 'BBAS3', 'BBDC3', 'BBDC4', 'BBRK3', 'BISA3', 'BOVA11', 'BRAP4', 'BRAX11', 'BRFS3', 'BRKM5', 'BRML3', 'BRPR3', 'BRSR6', 'BTOW3', 'BVMF3', 'CCRO3', 'CESP6', 'CIEL3', 'CMIG4', 'CPFE3', 'CPLE6', 'CRUZ3', 'CSAN3', 'CSMG3', 'CSMO11', 'CSNA3', 'CTIP3', 'CYRE3', 'DASA3', 'DTEX3', 'ELET3', 'ELET6', 'ELPL4', 'EMBR3', 'ENBR3', 'EVEN3', 'EZTC3', 'FIBR3', 'FIND11', 'GETI4', 'GFSA3', 'GGBR4', 'GOAU4', 'GOLL4', 'HGTX3', 'HRTP3', 'HYPE3', 'ITSA4', 'ITUB4', 'JBSS3', 'KLBN4', 'LAME4', 'LIGT3', 'LLXL3', 'LREN3', 'MILA11', 'MMXM3', 'MOBI11', 'MPLU3', 'MPXE3', 'MRFG3', 'MRVE3', 'MULT3', 'MYPK3', 'NATU3', 'ODPV3', 'OGXP3', 'PCAR4', 'PDGR3', 'PETR3', 'PETR4', 'PIBB11', 'POMO4', 'POSI3', 'PSSA3', 'QGEP3', 'RADL3', 'RAPT4', 'RDCD3', 'RENT3', 'RSID3', 'SANB11', 'SBSP3', 'SMAL11', 'SULA11', 'SUZB5', 'TBLE3', 'TCSA3', 'TIMP3', 'TOTS3', 'TRPL4', 'UGPA3', 'USIM3', 'USIM5', 'VAGR3', 'VALE3', 'VALE5', 'VIVT4', 'WEGE3')

for(i in symbols)
{
  print(i)
  system(sprintf("wget -O %s \"http://www.google.com/finance/getprices?q=%s&i=60&f=d,o,h,l,c,v\"", i, i), wait=TRUE)
  system(sprintf("./parse_ggle_csv %s < %s > %s.csv", i, i, i))
  loadLocalCSV(i)
}
