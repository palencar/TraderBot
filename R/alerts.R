library("data.table")
library("config")
library("htmltools")

#' @export
chartAlerts <- function(alerts = NULL, mode = "none")
{
  if(is.null(alerts) || nrow(alerts) == 0)
    return(NULL)

  for(i in 1:nrow(alerts))
  {
    alert <- alerts[i,]

    if(alert$timeframe == "1D")
      symbol <- getSymbolsDaily(alert$symbol, adjust = c("split", "dividend"))
    else
      symbol <- getSymbolsIntraday(alert$symbol, alert$timeframe, adjust = c("split", "dividend"))

    print(sprintf("Chart [%s] [%s] [%s]: %s", alert$symbol, alert$timeframe, alert$date, alert$alert))

    if(!is.null(alert) && !is.null(symbol))
    {
      chartSymbols(symbol, dev="png", xres = 1850, timeFrame=alert[i]$timeframe, smaPeriod = getParameters(alert$timeframe, "trade")$smaPeriod, mode = mode)
    }

    base::rm(list = base::ls(pattern = alert$symbol, envir = .GlobalEnv), envir = .GlobalEnv)
  }
}

getAlertSignals <- function(symbol, timeFrame)
{
  alerts <- getAlerts(symbols = symbol, openOnly = FALSE)
  alerts <- alerts[alerts$timeframe == timeFrame]

  if(nrow(alerts) == 0)
    return(NULL)

  alerts <- data.table(alerts[order(datetime, decreasing = TRUE)], key=c("symbol", "timeframe", "datetime"))
  alerts <- alerts[,transform(.SD, last=lastPrice(symbol)), by="symbol"]
  alerts <- alerts[,transform(.SD, adj.price={
    op <- rbind(xts(price, order.by = as.POSIXct(datetime)), xts(last.close, order.by = as.POSIXct(last.datetime)))
    round(as.numeric(adjustOperations(symbol, op)[datetime]), digits = 2)
  }), by=key(alerts)]

  signals <- NULL

  buy <- alerts[alerts$alert == "buy"]
  if(nrow(buy) > 0)
  {
    if(timeFrame == "1D")
      buy <- xts(buy$adj.price, order.by = as.Date(buy$datetime))
    else
      buy <- xts(buy$adj.price, order.by = as.POSIXct(buy$datetime))

    objName <- paste("Buy", symbol, timeFrame, sep = "_")
    signals <- c(signals, sprintf("addTA(%s, on = 1, col = 'blue', type = 'p', lwd = 1, pch=19)", objName))

    assign(objName, buy, .GlobalEnv)
  }

  sell <- alerts[alerts$alert == "sell"]
  if(nrow(sell) > 0)
  {
    if(timeFrame == "1D")
      sell <- xts(sell$adj.price, order.by = as.Date(sell$datetime))
    else
      sell <- xts(sell$adj.price, order.by = as.POSIXct(sell$datetime))

    objName <- paste("Sell", symbol, timeFrame, sep = "_")
    signals <- c(signals, sprintf("addTA(%s, on = 1, col = 'red', type = 'p', lwd = 1, pch=19)", objName))

    assign(objName, sell, .GlobalEnv)
  }

  return(signals)
}

getAlertsResults <- function(alerts)
{
  if(nrow(alerts) == 0)
    return(alerts)

  alerts <- data.table(alerts[order(datetime, decreasing = TRUE)], key=c("symbol", "timeframe", "datetime"))
  alerts <- alerts[,transform(.SD, last=lastPrice(symbol)), by=c("symbol", "timeframe")]
  alerts <- alerts[,transform(.SD, adj.price={
    op <- rbind(xts(price, order.by = as.POSIXct(datetime)), xts(last.close, order.by = as.POSIXct(last.datetime)))
    round(as.numeric(adjustOperations(symbol, op)[datetime]), digits = 2)
  }), by=c("symbol", "timeframe")]

  pr <- alerts$price
  lp <- alerts$last.close
  adj.pr <- alerts$adj.price

  alerts$alert      <- as.vector(alerts$alert)
  alerts$profit     <- round(ifelse(as.vector(alerts$alert) == "buy", lp-pr, -(lp-pr)), digits = 2)
  alerts$profit_perc<- round(ifelse(as.vector(alerts$alert) == "buy", (lp-pr)/pr, -(lp-pr)/pr)*100, digits = 2)
  alerts$adj.profit     <- round(ifelse(as.vector(alerts$alert) == "buy", lp-adj.pr, -(lp-adj.pr)), digits = 2)
  alerts$adj.profit_perc<- round(ifelse(as.vector(alerts$alert) == "buy", (lp-adj.pr)/adj.pr, -(lp-adj.pr)/adj.pr)*100, digits = 2)

  na.omit(alerts)[order(-datetime)]
}

sendAlert <- function(alerts)
{
  config <- config::get()

  print(alerts)

  datetime <- Sys.time()
  symbols <- as.vector(alerts$symbol)

  alerts <- getAlertsResults(alerts)

  report <- tagList(tags$h3("TraderBot Alert:"),
                    tags$html(tags$head(),
                              tagList(tags$p(datetime)),
                              tagList(apply(alerts, 1,
                                            function(x) {
                                              tags$p(
                                                tagList(
                                                  tags$a(href=paste0(config$alert$baseurl, x['symbol'], ".", x['timeframe'], ".png"),
                                                         paste(x['symbol'], x['timeframe'], "[", x['datetime'], "] Signal:", x['alert'], "Price: ", x['price'], "Adj. Price: ", x['adj.price'], " Last: ", x['last.close'], "Profit %: ", x['profit_perc'], "Profit (Adj) %: ", x['adj.profit_perc'])
                                                  )))
                                              }))
                    ))

  htmlOutput <- "index.html"

  save_html(report, htmlOutput)

  sysCmd <- NULL

  if(config$alert$type == "s3")
  {
    source <- paste0("charts/", paste(alerts$symbol, alerts$timeframe, sep = "."), ".png", sep = "")
    source <- c(source, htmlOutput)
    sysCmd <- c(sysCmd, paste0("aws s3 cp ", source, " s3://", config$alert$target,"/"))
  }

  if(config$alert$type == "mail")
  {
    for(symbol in alerts$symbol)
      imgAttachmets <- sprintf("-a charts/%s.png", symbol)

    sysCmd <- c(sysCmd, sprintf(paste0("mutt -e \"set content_type=text/html\" %s -s \"Trader Bot Alert\" < ", htmlOutput), config$alert$target))
  }

  for(cmd in sysCmd)
    system(cmd, intern=TRUE, ignore.stderr=TRUE)
}
