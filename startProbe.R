library('quantmod')
source('mysql_stocks.R')

startProbe <-
function()
{
  #system("beancounter update")
  
  symbolNames <- c("CPFE3.SA", "EQTL3.SA")

  symbolNamesObj <- getSymbolsMySQL(symbolNames, env = .GlobalEnv, user = 'paulo', dbname = 'beancounter')
  
  #str(symbolNames)
  return (symbolNamesObj)
}
