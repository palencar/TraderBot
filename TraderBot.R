source("startProbe.R")
source("filters.R")
source("polyReg.R")
source("chart.R")
library(parallel)
library(doParallel)

cl <- makeCluster(detectCores())
registerDoParallel(cl)

stopdtime <- "18:20:00"
fsmState <- "startProbe"

args <- commandArgs(trailingOnly=TRUE)
print(args)

stream = FALSE
Symbols = NULL

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
    
    AllSymbols <- startProbe(minAge=730, update=FALSE)
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
        
        alertR = tryCatch({
          computeRegressions(symbol, as.Date(dt), as.Date(dt))
        }, warning = function(war) {
          print(war)
          return(NULL)
        }, error = function(err) {
          print(err)
          return(NULL)
        }, finally={
        })    
        
        alertL = tryCatch({
          filterLRI(linearRegressionIndicator(symbol)[sprintf("/%s", as.Date(dt))], threshold=1.2)
        }, warning = function(war) {
          print(war)
          return(NULL)
        }, error = function(err) {
          print(err)
          return(NULL)
        }, finally={
        })
        
        obj <- get(symbol)
        seq <- as.double((Hi(obj)+Lo(obj)+Cl(obj))/3)
        sma <- SMA(seq, n=200)
        ssd <- sd(as.double(na.omit(seq-sma)))
        
        alertS <- FALSE
        if(last(seq) > (last(sma) + (2*ssd)) || last(seq) < (last(sma) - (2*ssd)))  
          alertS <- TRUE
        
        if(!is.null(alertR))
          print(sprintf("%s %s: alertR %s", as.Date(dt), symbol, alertR))
        
        if(!is.null(alertL))
          print(sprintf("%s %s: alertL %s", as.Date(dt), symbol, alertL))
        
        print(sprintf("%s %s: alertS %s", as.Date(dt), symbol, alertS))

        obj <- get(symbol)
        lsma <- last(SMA(as.double((Hi(obj)+Lo(obj)+Cl(obj))/3), n=200))
        lst <- last(as.double((Hi(obj)+Lo(obj)+Cl(obj))/3))
        
        if(!is.null(alertR) && !(symbol %in% alertSymbols))
        {
          alertSymbols <- c(alertSymbols, symbol)
        }
        
        if(!is.null(alertL) && !(symbol %in% alertSymbols))
        {
          if((alertL == "down" && lsma < lst) || (alertL == "up" && lsma > lst))
            alertSymbols <- c(alertSymbols, symbol)
        }
        
        if(alertS && !(symbol %in% alertSymbols))
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

toFilter <- NULL

while(fsmState != "end")
{
  print(fsmState)
  
  if(fsmState == "startProbe")
  {
    AllSymbols <- startProbe(minAge=730)
    
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
    
    if(is.null(toFilter))
    {
      toFilter <- setdiff(AllSymbols, Symbols)
      Rprof("filterIncomplete.out")
      accepted <- filterIncomplete(toFilter)
      Rprof(NULL)
      Symbols <- union(accepted, Symbols)
    }
    
    print("COMPUTING:")
    print(Symbols)
    
    alertSymbols <- NULL
    
    for(symbol in Symbols)
    {
      print(symbol)
     
      alertR = tryCatch({
        computeRegressions(symbol, startDate, endDate)
      }, warning = function(war) {
        print(war)
        return(NULL)
      }, error = function(err) {
        print(err)
        return(NULL)
      }, finally={
      })    

      alertL = tryCatch({
        filterLRI(linearRegressionIndicator(symbol)[sprintf("/%s", endDate)], threshold=1.2)
      }, warning = function(war) {
        print(war)
        return(NULL)
      }, error = function(err) {
        print(err)
        return(NULL)
      }, finally={
      })
      
      obj <- get(symbol)
      seq <- as.double((Hi(obj)+Lo(obj)+Cl(obj))/3)
      sma <- SMA(seq, n=200)
      ssd <- sd(as.double(na.omit(seq-sma)))
      
      alertS <- FALSE
      if(last(seq) > (last(sma) + (2*ssd)) || last(seq) < (last(sma) - (2*ssd)))  
        alertS <- TRUE
      
      if(!is.null(alertR))
        print(sprintf("%s %s: alertR %s", as.Date(endDate), symbol, alertR))
      
      if(!is.null(alertL))
        print(sprintf("%s %s: alertL %s", as.Date(endDate), symbol, alertL))
      
      print(sprintf("%s %s: alertS %s", as.Date(endDate), symbol, alertS))
      
      obj <- get(symbol)
      lsma <- last(SMA(as.double((Hi(obj)+Lo(obj)+Cl(obj))/3), n=200))
      lst <- last(as.double((Hi(obj)+Lo(obj)+Cl(obj))/3))
      
      if(!is.null(alertR) && !(symbol %in% alertSymbols))
      {
        alertSymbols <- c(alertSymbols, symbol)
      }
      
      if(!is.null(alertL) && !(symbol %in% alertSymbols))
      {
        if((alertL == "down" && lsma < lst) || (alertL == "up" && lsma > lst))
          alertSymbols <- c(alertSymbols, symbol)
      }
      
      if(alertS && !(symbol %in% alertSymbols))
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
    
    for(symbol in alertSymbols)
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

      wal <- wallet()
      if(alertSymbols %in% wal)
        muttCmd <- sprintf("echo \"%s\" | mutt -s \"Trader Bot Alert W\" pbalencar@yahoo.com %s", sprintf("Snapshot time: %s", startTime), paste(imgAttachmets, collapse=" "))
      else
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
