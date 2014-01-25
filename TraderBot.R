source("startProbe.R")
source("filters.R")
source("polyReg.R")
source("chart.R")


require(doMC)
registerDoMC()

stopdtime <- "18:20:00"
fsmState <- "startProbe"

args <- commandArgs(trailingOnly=TRUE)
print(args)

#Rprof("profile_tb.out")

stream = FALSE
Symbols <- NULL

if(length(args) > 0)
{
  if(args[1] == "stream")
  {
    stream = TRUE
    
    if(length(args) > 1)
    {
      Symbols <- tail(args, n=(length(args)-1))
    }
  }
  else if(args[1] == "compute")
  {
    if(length(args) >= 3)
    {
      startDate <- args[2]
      endDate <- args[3]
    }
    else if(length(args) == 2)
    {
      startDate <- endDate <- args[2]
    }
    else
    {
      startDate <- endDate <- Sys.Date()
    }
    
    AllSymbols <- startProbe()
    Symbols <- c()
    
    if(length(args) > 3)
    {
      for(i in 4:length(args))
      {
        Symbols[i-3] <- args[i]
      }
    }
    else
    {
      Symbols <- AllSymbols
    }
    
    print(args)
    alert <- computeRegressions(Symbols, startDate, endDate)
    if(is.null(alert) == FALSE)
    {
      print(alert) 
    }
    
    quit()
  }
}

while(fsmState != "end")
{
  print(fsmState)
  
  if(fsmState == "startProbe")
  {
    Symbols <- startProbe(symbolNames=Symbols)
    
    print("COMPUTING:")
    print(Symbols)
    
    fsmState <- "computeRegressions"
  }
  else if(fsmState == "computeRegressions")
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
    
    alertSymbols <- c()
    i <- 1
    
    Symbols <- filterIncomplete(Symbols)
    
    for(symbol in Symbols)
    {
      alertR <- computeRegressions(symbol, startDate, endDate)
      alertL <- filterLRI(get(symbol), linearRegressionIndicator(symbol)[sprintf("/%s", endDate)], threshold=1.2)
      
      if(is.null(alertR) == FALSE || is.null(alertL) == FALSE)
      {
        alertSymbols[[i]] <- symbol
        i <- i + 1
      }
    }
    
    fsmState <- "plotWallet"
  }
  else if(fsmState == "plotWallet")
  {
    if(stream == FALSE)
      chartSymbols(wallet(), dev="png")
    
    fsmState <- "plotAlerts"
  }
  else if(fsmState == "plotAlerts")
  {
    if(stream == TRUE)
      print(sprintf("Plot [%s]: %s", alertSymbols, startTime))
    
    chartSymbols(alertSymbols, dev="png")
    
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
        imgAttachmets <- sprintf("-a charts/%s.png", alertSymbols)
      
      muttCmd <- sprintf("echo \"%s\" | mutt -s \"Trader Bot Alert\" pbalencar@yahoo.com %s", sprintf("Snapshot time: %s", startTime), paste(imgAttachmets, collapse=" "))
      
      cmdOut <- system(muttCmd, intern=TRUE, ignore.stderr=TRUE)
    }
    
    fsmState <- "startProbe"
  }
}

#Rprof(NULL)
