library("data.table")
library("config")
library("htmltools")

#' @export
addAlerts <- function(symbol, datetime, alert, price, timeframe)
{
  alerts <- data.frame(symbol, timeframe, datetime, alert, price)

  query <- paste("REPLACE INTO alerts (symbol, timeframe, datetime, alert, price) VALUES",
                 paste(sprintf("('%s', '%s', '%s', '%s', %s)", alerts$symbol, alerts$timeframe, alerts$date, alerts$alert, alerts$price), collapse=', '))

  getQuery(query)
}

#' @export
getAlerts <- function(n = 50)
{
  alerts <- getQuery("select * from alerts order by datetime desc")

  return(head(alerts[!duplicated(alerts[,c('symbol','alert')]),], n))
}

#' @export
chartAlerts <- function(alerts = NULL, parameters)
{
  if(is.null(alerts) || nrow(alerts) == 0)
    return(NULL)

  for(i in 1:nrow(alerts))
  {
    alert <- alerts[i,]

    if(alert$timeframe == "1D")
      symbol <- getSymbolsDaily(alert$symbol)
    else
      symbol <- getSymbolsIntraday(alert$symbol, alert$timeframe, updateLast = FALSE)

    print(sprintf("Chart [%s] [%s] [%s]: %s", alert$symbol, alert$timeframe, alert$date, alert$alert))

    if(!is.null(alert))
    {
      chartSymbols(symbol, dev="png", xres = 1850, smaPeriod = ifelse(!is.null(parameters), parameters$smaPeriod, 400))
    }

    base::rm(list = base::ls(pattern = alert$symbol, envir = .GlobalEnv), envir = .GlobalEnv)
  }
}

sendAlert <- function(alerts)
{
  config <- config::get()

  print(alerts)

  datetime <- Sys.time()
  symbols <- as.vector(alerts$symbol)

  pr <- alerts$price
  lp <- unlist(lapply(symbols, function(x) {df <- lastPrice(x); df$close} ))

  alerts$date       <- as.character(alerts$date)
  alerts$alert      <- as.vector(alerts$alert)
  alerts$price      <- round(pr, digits = 2)
  alerts$lastprice  <- round(lp, digits = 2)
  alerts$profit    <- formatC(ifelse(as.vector(alerts$alert) == "buy", lp-pr, -(lp-pr)), digits = 3, format = "f")
  alerts$profit_pp <- formatC(ifelse(as.vector(alerts$alert) == "buy", (lp-pr)/pr, -(lp-pr)/pr), digits = 3, format = "f")

  alerts <- na.omit(alerts)

  report <- tagList(tags$h3("TraderBot Alert:"),
                    tags$html(tags$head(),
                              tagList(tags$p(datetime)),
                              tagList(apply(alerts, 1,
                                            function(x) {
                                              tags$p(
                                                tagList(
                                                  tags$a(href=paste0(config$alert$baseurl, x['symbol'], ".", x['timeframe'], ".png"),
                                                         paste(x['symbol'], x['timeframe'], "[", x['date'], "] Signal:", x['alert'], "Price: ", x['price'], " Last: ", x['lastprice'], "Profit: ", x['profit_pp'], "%")
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
    for(symbol in symbols)
      imgAttachmets <- sprintf("-a charts/%s.png", symbol)

    sysCmd <- c(sysCmd, sprintf(paste0("mutt -e \"set content_type=text/html\" %s -s \"Trader Bot Alert\" < ", htmlOutput), config$alert$target))
  }

  for(cmd in sysCmd)
    system(cmd, intern=TRUE, ignore.stderr=TRUE)
}
