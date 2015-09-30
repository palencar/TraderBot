
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
        seql = tail(seq, 2)
        smal = tail(sma, 2)
        
        if(seql[2] > (smal[2] + (2*ssd)) && seql[1] <= (smal[1] + (2*ssd)))  
          alertS <- "upper"
        
        if(seql[2] < (smal[2] - (2*ssd)) && seql[1] >= (smal[1] - (2*ssd)))  
          alertS <- "lower"
        
        #TODO utilizar valor Hi e Lo em vez da media
        sdp <- (seql[2]-smal[2])/ssd
        
        alertA <- FALSE
        alertB <- FALSE
        #TODO utilizar valor Hi e Lo em vez da media
        objOHLC <- obj[paste(rev(seq(as.Date(endDate), length=2, by="-4 years")),collapse = "::")]
        objLen <- length(index(objOHLC))
        totAb <- length(which(Hi(objOHLC) > as.double(Hi(tail(obj, 1)))))
        totBl <- length(which(Lo(objOHLC) < as.double(Lo(tail(obj, 1)))))
        
        if((totAb/objLen) < 0.1)  #10%
          alertA <- totAb/objLen
        
        if((totBl/objLen) < 0.1)  #10%
          alertB <- totBl/objLen
        
        if(!is.null(alertR))
          print(sprintf("%s %s: alertR %s", as.Date(endDate), symbol, alertR))
        
        if(!is.null(alertL))
          print(sprintf("%s %s: alertL %s", as.Date(endDate), symbol, alertL))
        
        print(sprintf("%s %s: alertS %s", as.Date(endDate), symbol, alertS))
        
        if(alertA != FALSE)
          print(sprintf("%s %s: alertA %s", as.Date(endDate), symbol, alertA))
        
        if(alertB != FALSE)
          print(sprintf("%s %s: alertB %s", as.Date(endDate), symbol, alertB))
        
        logLine <- paste(as.Date(endDate), paste(as.double(tail(obj, 1)), collapse = " "), alertR, alertL, alertS, alertA, alertB, sdp)
        logFile <- paste("training/",symbol,".log", sep = "")
        cat(logLine, file=logFile, sep = "\n", append=TRUE)
        
        #TODO armazenar os alertas para cada simbolo no dia
        #TODO enviar por email a descricao dos alertas
        
        obj <- get(symbol)
        lsma <- last(SMA(as.double((Hi(obj)+Lo(obj)+Cl(obj))/3), n=200))
        lst <- last(as.double((Hi(obj)+Lo(obj)+Cl(obj))/3))
        
        if(!is.null(alertR) && alertR != FALSE && !(symbol %in% alertSymbols))
        {
          alertSymbols <- c(alertSymbols, symbol)
        }
        
        if(!is.null(alertL) && alertL != FALSE && !(symbol %in% alertSymbols))
        {
          if((alertL == "down" && lsma < lst) || (alertL == "up" && lsma > lst))
            alertSymbols <- c(alertSymbols, symbol)
        }
        
        if(alertS != FALSE && !(symbol %in% alertSymbols))
        {
          alertSymbols <- c(alertSymbols, symbol)
        }
        
        #if((alertA != FALSE || alertB != FALSE) && !(symbol %in% alertSymbols))
        #{
        #  alertSymbols <- c(alertSymbols, symbol)
        #}
        if(symbol %in% alertSymbols)
        {
          alertLog <- paste(alertLog, paste(symbol, logLine, collapse = " "), sep = "\n")
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
      if(stream == TRUE)
        print(sprintf("Chart [%s]: %s", alertSymbols, startTime))
      
      if(length(alertSymbols) > 0)
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
        if(length(intersect(alertSymbols,wal)) > 0)
          muttCmd <- sprintf("echo \"%s\" | mutt -s \"Trader Bot Alert W\" pbalencar@yahoo.com %s", paste(sprintf("Snapshot time: %s", startTime), alertLog, collapse = "\n"), paste(imgAttachmets, collapse=" "))
        else
          muttCmd <- sprintf("echo \"%s\" | mutt -s \"Trader Bot Alert\" pbalencar@yahoo.com %s", paste(sprintf("Snapshot time: %s", startTime), alertLog, collapse = "\n"), paste(imgAttachmets, collapse=" "))
        
        cmdOut <- system(muttCmd, intern=TRUE, ignore.stderr=TRUE)
      }
      
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