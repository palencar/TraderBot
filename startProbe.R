library('quantmod')
source('mysql_stocks.R')

startProbe <-
function()
{
  system("beancounter update")
  
  symbolNames <- c("CPFE3.SA", "ELPL4.SA", "TBLE3.SA", "CMIG4.SA",
                   "TRPL4.SA", "MPXE3.SA", "COCE3.SA", "ENBR3.SA", "LIGT3.SA",
                   "EQTL3.SA", "OGXP3.SA", "PETR4.SA", "BBAS3.SA")

  symbolNamesObj <- getSymbolsMySQL(symbolNames, env = .GlobalEnv, user = 'paulo', dbname = 'beancounter')
  
  #str(symbolNames)
  return (symbolNamesObj)
}
