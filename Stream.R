
computeStream <- function(Symbols)
{
  stopdtime <- "18:20:00"
  fsmState <- "startProbe"
  toFilter <- NULL
  
  while(fsmState != "end")
  {
    print(fsmState)
    
    if(fsmState == "startProbe")
    {
      AllSymbols <- startProbe(minAge=200)
      
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
        toFilter <- filterVolume(toFilter)
        accepted <- filterIncomplete(toFilter)
        Symbols <- union(accepted, Symbols)
      }
      
      print("COMPUTING:")
      print(Symbols)
      
      alertSymbols <- NULL
      alertLog <- NULL
      
      dt <- endDate
      
      for(symbol in Symbols)
      {
        print(symbol)
        
        decision <- trade(symbol, dt)
        
        if(decision != "hold")
        {
          alertSymbols <- c(alertSymbols, symbol)
          
          price <- sprintf("%.2f", sum(HLC(get(symbol)[as.Date(dt)]))/3)
          logLine <- paste(symbol, as.Date(dt), decision, price, collapse = " ")
          logFile <- paste("training/",symbol,".log", sep = "")
          cat(logLine, file=logFile, sep = "\n", append=TRUE)
          cmdLine <- sprintf("cat training/%s.log | grep -v \"0.00\" | sort -u > training/%s.bkp && mv training/%s.bkp training/%s.log", symbol, symbol, symbol, symbol)
          system(cmdLine)
          alertLog <- paste(alertLog, logLine, sep = "\n")
        }
      }
      
      alertsFile <- "data/alerts.rds"
      if(file.exists(alertsFile))
        alerts <- readRDS(alertsFile)
      
      for(symbol in alertSymbols)
      {
        alerts <- c(alerts[alerts != symbol], symbol)
      }
      
      saveRDS(alerts, file=alertsFile)
      
      fsmState <- "chartAlerts"
    }
    else if(fsmState == "chartAlerts")
    {
      print(sprintf("Chart [%s]: %s", alertSymbols, startTime))
      
      if(length(alertSymbols) > 0)
        chartSymbols(alertSymbols, dev="png")
      
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
        if(length(intersect(alertSymbols,wal)) > 0)
          muttCmd <- sprintf("echo \"%s\" | mutt -s \"Trader Bot Alert W\" pbalencar@yahoo.com %s", paste(sprintf("Snapshot time: %s", startTime), alertLog, collapse = "\n"), paste(imgAttachmets, collapse=" "))
        else
          muttCmd <- sprintf("echo \"%s\" | mutt -s \"Trader Bot Alert\" pbalencar@yahoo.com %s", paste(sprintf("Snapshot time: %s", startTime), alertLog, collapse = "\n"), paste(imgAttachmets, collapse=" "))
        
        cmdOut <- system(muttCmd, intern=TRUE, ignore.stderr=TRUE)
      }
      
      if(stream == FALSE)
        fsmState <- "end"
      else
        fsmState <- "startProbe"
    }
    else if(fsmState == "end")
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