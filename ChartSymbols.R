source("dbInterface.R")
source("filters.R")
source("polyReg.R")
source("chart.R")
source("orders.R")


args_cmd <- commandArgs(trailingOnly=TRUE)

symbolNames <- NULL

if(length(args_cmd) >= 1)
{
  if(args_cmd[1] == "wallet")
    symbolNames <- getWallet()
  else if(args_cmd[1] == "alerts")
  {
    alerts <- NULL
    alertsFile <- "data/alerts.rds"

    if(file.exists(alertsFile))
      alerts <- readRDS(alertsFile)

    symbolNames <- alerts
  }
  else
    symbolNames <- args_cmd
}


Symbols <- startProbe(symbolNames, FALSE)

Symbols <- filterVolume(Symbols)
filterSymbols <- filterIncomplete(Symbols)
#filterSymbols <- Symbols 

for(symbol in filterSymbols)
{
  chartSymbols(symbol, dev="png")
  
  tryCatch({
    chartSymbols(Symbols=symbol, period="5 years", timeFrame = "weekly", dev = "png", path = "chart-weekly/")
  }, warning = function(war) {
    print(war)
    return(NULL)
  }, error = function(err) {
    print(err)
    return(NULL)
  }, finally={
  })
}
