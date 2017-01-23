library("data.table")

#' @export
addAlerts <- function(symbol, date, alert = NA)
{
  if(is.null(symbol))
    return()

  alerts <- NULL
  alertsFile <- "datacache/alerts.rds"
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
getAlerts <- function(n = 20, date = NULL)
{
  alerts <- NULL
  alertsFile <- "datacache/alerts.rds"
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
