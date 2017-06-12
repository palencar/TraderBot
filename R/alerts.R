library("data.table")
library("config")
library("htmltools")

#' @export
addAlerts <- function(symbol, date, alert = NA, timeFrame = "1D")
{
  if(is.null(symbol))
    return()

  alerts <- NULL
  alertsFile <- paste0("datacache/alerts-", timeFrame, ".rds")
  if(file.exists(alertsFile))
  {
    alerts <- readRDS(alertsFile)
  }

  df <- data.frame(symbol, date, alert)
  df <- rbindlist(list(df, alerts), fill = TRUE)
  df <- unique(df)
  df <- df[order(df$date, decreasing = TRUE),]

  saveRDS(df, file=alertsFile)
}

#' @export
getAlerts <- function(n = 20, date = NULL, timeFrame = "1D")
{
  alerts <- NULL
  alertsFile <- paste0("datacache/alerts-", timeFrame, ".rds")
  if(file.exists(alertsFile))
  {
    alerts <- readRDS(alertsFile)
  }

  if(!is.null(date))
  {
    alerts <- alerts[which(alerts$date >= date)]
  }

  alerts <- alerts[order(alerts$date, decreasing = TRUE),]

  df <- head(alerts, n = n)

  return(df)
}

sendAlert <- function(alerts, timeFrame = "1D")
{
  config <- config::get()

  datetime <- Sys.time()
  symbols <- as.vector(alerts$symbol)

  report <- tagList(tags$h3(paste0("TraderBot Alert: ", timeFrame)),
                    tags$html(tags$head(),
                              tagList(tags$p(datetime)),
                              tagList(apply(alerts, 1, function(x) {
                              tagList(tags$p(paste0(x['symbol'], " [", x['date'], "]: ", x['alert'])),
                                      tags$img(src=paste0(config$alert$baseurl, x['symbol'],".png"))) }))
                    ))

  htmlOutput <- ifelse(timeFrame == "1D", "index.html", paste0("index-", timeFrame, ".html"))

  save_html(report, htmlOutput)

  sysCmd <- NULL

  if(config$alert$type == "s3")
  {
    source <- paste0("charts/", symbols, ".png", sep = "")
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
