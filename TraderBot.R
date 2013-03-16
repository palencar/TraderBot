source("startProbe.R")
source("filters.R")
source("poly-reg.R")
source("plot.R")


fsmState <- "startProbe"

#args <- commandArgs(trailingOnly=TRUE)
#print(args)

while(fsmState != "end")
{
  print(fsmState)
  
  if(fsmState == "startProbe")
  {
    Symbols <- startProbe()
    
    fsmState <- "processRegressions"
  }
  else if(fsmState == "processRegressions")
  {
    #startDate <- format(Sys.time(), "%Y-%m-%d")
    #select date from stockprices order by date desc limit 1;
    startDate <- "2013-03-15"
    endDate <- startDate
    
    processRegressions(Symbols, startDate, endDate)
    
    fsmState <- "filterRevert"
  }
  else if(fsmState == "filterRevert")
  {
    #startDate <- format(Sys.time(), "%Y-%m-%d")
    #endDate <- startDate
   
    alertSymbols <- filterObjectsSets(Symbols, startDate, endDate)
    
    fsmState <- "plotWallet"
  }
  else if(fsmState == "plotWallet")
  { 
    plotRegressions(wallet())
    
    fsmState <- "plotAlerts"
  }
  else if(fsmState == "plotAlerts")
  {
    plotRegressions(alertSymbols)
    
    fsmState <- "startProbe" 
  }
  #else if(fsmState == "sendMail")
  #{
  #}
}
