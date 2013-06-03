source("startProbe.R")
source("filters.R")
source("poly-reg.R")
source("plot.R")


stopdtime <- "18:20:00"
fsmState <- "startProbe"

args <- commandArgs(trailingOnly=TRUE)
print(args)

stream = FALSE

if(length(args) > 0)
  if(args[1] == "stream")
    stream = TRUE

#Rprof("profile_tb.out")

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
    lastSession <- lastTradingSession()
    startDate <- as.Date(lastSession) + 1
    
    if(startDate > format(Sys.time(), "%Y-%m-%d"))
      startDate <- format(Sys.time(), "%Y-%m-%d")
    
    endDate <- format(Sys.time(), "%Y-%m-%d")
    
    #today isn't a session day
    if(lastSession < endDate)
      stream <- FALSE
    
    startTime <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    
    processRegressions(Symbols, startDate, endDate)
    
    fsmState <- "filterRevert"
  }
  else if(fsmState == "filterRevert")
  {
    alertSymbols <- filterObjectsSets(Symbols, startDate, endDate)
    
    fsmState <- "plotWallet"
  }
  else if(fsmState == "plotWallet")
  {
    if(stream == FALSE)
      plotRegressions(wallet())
    
    fsmState <- "plotAlerts"
  }
  else if(fsmState == "plotAlerts")
  {
    if(stream == TRUE)
      print(sprintf("Plot [%s]: %s", alertSymbols, startTime))
    
    plotRegressions(alertSymbols)
    
    if(stream == FALSE)
      fsmState <- "end"
    else
      fsmState <- "sendMail"
    
    dtime <- format(Sys.time(), "%H:%M:%S")
    
    if(dtime > stopdtime)
      stream <- FALSE
  }
  else if(fsmState == "sendMail")
  {
    if(length(alertSymbols) > 0)
    {
      for(i in alertSymbols)
        imgAttachmets <- sprintf("-a plots/%s.png", alertSymbols)
      
      muttCmd <- sprintf("echo \"%s\" | mutt -s \"Trader Bot Alert\" pbalencar@yahoo.com %s", sprintf("Snapshot time: %s", startTime), paste(imgAttachmets, collapse=" "))
      
      cmdOut <- system(muttCmd, intern=TRUE, ignore.stderr=TRUE)
    }
    
    fsmState <- "startProbe"
  }
}

#Rprof(NULL)
