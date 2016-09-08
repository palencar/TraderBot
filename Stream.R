source("result.R")

computeStream <- function(Symbols)
{
  stopdtime <- "18:20:00"
  fsmState <- "startProbe"
  toFilter <- NULL
  tradeAlerts <- NULL
  
  while(fsmState != "end")
  {
    print(fsmState)
    
    if(fsmState == "startProbe")
    {
      AllSymbols <- startProbe(minAge=720)
      
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
      
      dt <- lastSession
      
      startTime <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      startDay <- format(Sys.time(), "%Y-%m-%d")
      
      toFilter <- setdiff(AllSymbols, Symbols)
      accepted <- filterData(toFilter, endDate)
      Symbols <- union(accepted, Symbols)
      
      print("COMPUTING:")
      print(Symbols)
      
      alertSymbols <- NULL
      alertLog <- NULL
      
      for(i in seq(0, 2, 0.1))
      for(j in seq(0, -2, -0.1))
      for(symbol in Symbols)
      {
        tradeDecision <- trade(symbol, as.Date(dt), i, i)
        tradeAlert <- sprintf("%s%s%s", symbol, tradeDecision$decision, tradeDecision$reason)
        
        if(tradeDecision$decision != "hold" && (tradeAlert %in% tradeAlerts) == FALSE)
        {
          alertSymbols <- c(alertSymbols, symbol)
          tradeAlerts <- c(tradeAlert, tradeAlerts)
          
          price <- sprintf("%.2f", as.numeric(lastPrice(symbol)))
          logLine <- paste(symbol, as.Date(dt), tradeDecision$decision, price, collapse = " ")
          
          writeResult(symbol, logLine, c(sprintf("%1.1f", i), sprintf("%1.1f", j)))
          
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
        
        wal <- getWallet()
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
        imagePath <- "chart-history"
        imageName <- sprintf("%s/%s", imagePath, symbol)
        dir.create(imagePath, showWarnings=FALSE)
        chartSymbols(Symbols=symbol, dev="png", path=imagePath, suffix=startDay)
      }
    }
  }
}