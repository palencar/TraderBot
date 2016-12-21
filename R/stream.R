source("R/result.R")
source("R/alerts.R")

#' @export
computeStream <- function(Symbols = NULL, openMarket = TRUE)
{
  stopdtime <- "18:20:00"
  fsmState <- "startProbe"
  tradeAlerts <- NULL

  while(fsmState != "end")
  {
    print(fsmState)

    if(fsmState == "startProbe")
    {
      AllSymbols <- startProbe(symbolNames=Symbols, minAge=720)

      fsmState <- "computeRegressions"
    }
    else if(fsmState == "computeRegressions")
    {
      lastSession <- as.Date(lastTradingSession())
      startDate <- as.Date(lastSession) + 1

      if(startDate > Sys.Date())
        startDate <- Sys.Date()

      endDate <- Sys.Date()

      if(lastSession < endDate)
        openMarket <- FALSE

      tradeDate <- lastSession

      startTime <- Sys.time()
      startDay <- Sys.Date()

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

        tradeDecisions <- trade(symbol, tradeDate, smaPeriod = smaPeriod, upperBand = upperBand, lowerBand = lowerBand, upChange = upChange, downChange = downChange, price = price)

        for(tradeDecision in tradeDecisions)
        {
          print(paste(symbol, tradeDate, tradeDecision$decision, tradeDecision$reason))

          tradeAlert <- sprintf("%s%s%s", symbol, tradeDecision$decision, tradeDecision$reason)

          if(tradeDecision$decision != "hold" && (tradeAlert %in% tradeAlerts) == FALSE)
          {
            alertSymbols <- c(alertSymbols, symbol)
            tradeAlerts <- c(tradeAlert, tradeAlerts)

            price <- sprintf("%.2f", as.numeric(lastPrice(symbol)))
            logLine <- paste(symbol, tradeDate, tradeDecision$decision, price, collapse = " ")

            writeResult(symbol, logLine, "../stream")

            alertLog <- paste(alertLog, logLine, sep = "\n")
          }
        }
      }

      addAlerts(alertSymbols, tradeDate)

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
        openMarket <- FALSE
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

      if(openMarket == FALSE)
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
