source("R/result.R")

#' @export
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
      accepted <- filterDataM(toFilter, endDate)
      Symbols <- union(accepted, Symbols)

      print("COMPUTING:")
      print(Symbols)

      alertSymbols <- NULL
      alertLog <- NULL

      for(symbol in Symbols)
      {
        smaPeriod <- 200
        upperBand <- 0.5
        lowerBand <- -0.5
        upChange <- 0.5
        downChange <- -0.5

        price <- meanPrice(symbol)

        tradeDecisions <- trade(symbol, as.Date(dt), smaPeriod = smaPeriod, upperBand = upperBand, lowerBand = lowerBand, upChange = upChange, downChange = downChange, price = price)

        for(tradeDecision in tradeDecisions)
        {
          print(paste(symbol, Sys.Date(), tradeDecision$decision, tradeDecision$reason))

          tradeAlert <- sprintf("%s%s%s", symbol, tradeDecision$decision, tradeDecision$reason)

          if(tradeDecision$decision != "hold" && (tradeAlert %in% tradeAlerts) == FALSE)
          {
            alertSymbols <- c(alertSymbols, symbol)
            tradeAlerts <- c(tradeAlert, tradeAlerts)

            price <- sprintf("%.2f", as.numeric(lastPrice(symbol)))
            logLine <- paste(symbol, as.Date(dt), tradeDecision$decision, price, collapse = " ")

            writeResult(symbol, logLine, "../stream")

            alertLog <- paste(alertLog, logLine, sep = "\n")
          }
        }
      }

      alertsFile <- "datacache/alerts.rds"
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
          muttCmd <- sprintf("echo \"%s\" | mutt -s \"Trader Bot Alert W\" %s %s", paste(sprintf("Snapshot time: %s", startTime), alertLog, collapse = "\n"), paste(readLines("mail.addr"), collapse=" "), paste(imgAttachmets, collapse=" "))
        else
          muttCmd <- sprintf("echo \"%s\" | mutt -s \"Trader Bot Alert\" %s %s", paste(sprintf("Snapshot time: %s", startTime), alertLog, collapse = "\n"), paste(readLines("mail.addr"), collapse=" "), paste(imgAttachmets, collapse=" "))

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
