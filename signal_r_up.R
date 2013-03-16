source("startProbe.R")
source("filters.R")
source("poly-reg.R")
source("plot.R")
source("utils.R")

args <- c()
args[1] <- as.Date(Sys.Date() - 30)
args[2] <- as.Date(Sys.Date())

args_cmd <- commandArgs(trailingOnly=TRUE)

if(length(args_cmd) == 2)
{
  args <- args_cmd
}

Symbols <- startProbe()

k1 <- 10
k2 <- 730

for(dt in seq(as.Date(args[2]), as.Date(args[1]), by = "-1 day"))
{
  chartDate <- sprintf("%s", as.Date(dt))
  
  filterSymbols <- filterIncomplete(Symbols)

  for(symbol in filterSymbols)
  {
    if(length(get(symbol)[chartDate]) == 0)
      next
    
    strOut <- sprintf("filterRevert %s %d %d %s", symbol, k1, k2, chartDate)
    print(strOut)
    
    alertas <- findCurves(symbol, k1, k2, dateLimit=chartDate)
    
    if(length(alertas) > 0)
    {
      objectName <- sprintf("backtest/%s-%s_%d_%d.rds", chartDate, symbol, k1, k2)
      
      saveRDS(alertas, file=objectName)
    }
    if(length(alertas) == 0)
    {
      next
    }
  }
}