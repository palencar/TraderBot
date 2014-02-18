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
    
    Symbols <- filterIncomplete(Symbols)
    
    alertSymbols <- NULL
    
    for(dt in seq.Date(as.Date(startDate), as.Date(endDate), by="+1 days"))
    {
      for(symbol in Symbols)
      {
        print(symbol)
        
        alertR <- computeRegressions(symbol, as.Date(dt), as.Date(dt))
        alertL <- filterLRI(get(symbol)[sprintf("/%s", as.Date(dt))], linearRegressionIndicator(symbol)[sprintf("/%s", as.Date(dt))], threshold=1.2)
        
        if(is.null(alertR) == FALSE)
          print(sprintf("%s %s: alertR", as.Date(dt), symbol))
        
        if(alertL == TRUE)
          print(sprintf("%s %s: alertL", as.Date(dt), symbol))
        
        if(is.null(alertR) == FALSE || alertL == TRUE)
        {
          alertSymbols <- c(alertSymbols, symbol)
        }
      }
    }
    
    if(is.null(alertSymbols) == FALSE)
    {
      print(alertSymbols) 
    }
    
    quit()
  }
}

filter <- FALSE

while(fsmState != "end")
{
  print(fsmState)
  
  if(fsmState == "startProbe")
  {
    Symbols <- startProbe(symbolNames=Symbols)
    
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
    startDay <- format(Sys.time(), "%Y-%m-%d")
    
    if(filter == FALSE)
    {
      Symbols <- filterIncomplete(Symbols)
      filter <- TRUE
    }
    
    print("COMPUTING:")
    
    alertSymbols <- NULL
    
    for(symbol in Symbols)
    {
      print(symbol)
     
      alertR <- computeRegressions(symbol, startDate, endDate)
      alertL <- filterLRI(get(symbol)[sprintf("/%s", endDate)], linearRegressionIndicator(symbol)[sprintf("/%s", endDate)], threshold=1.2)
      
      if(is.null(alertR) == FALSE)
        print(sprintf("%s %s: alertR", as.Date(endDate), symbol))
      
      if(alertL == TRUE)
        print(sprintf("%s %s: alertL", as.Date(endDate), symbol))
      
      if(is.null(alertR) == FALSE || alertL == TRUE)
      {
        alertSymbols <- c(alertSymbols, symbol)
      }
    }
    
    fsmState <- "chartAlerts"
  }
  else if(fsmState == "chartAlerts")
  {
    if(stream == TRUE)
      print(sprintf("Chart [%s]: %s", alertSymbols, startTime))
    
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
  else if(fsmState == "end")
  {
    if(stream == FALSE)
    {
      for(symbol in Symbols)
      {
        imagePath <- sprintf("chart-history/%s", symbol)
        dir.create(imagePath, showWarnings=FALSE)
        chartSymbols(Symbols=symbol, dev="png", path=imagePath, suffix=startDay)
      }
    }
  }
}

#Rprof(NULL)
