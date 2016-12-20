addAlerts <- function(symbol, date)
{
  if(is.null(symbol))
    return()

  alerts <- NULL
  alertsFile <- "datacache/alerts.rds"
  if(file.exists(alertsFile))
  {
    alerts <- readRDS(alertsFile)
  }

  df <- data.frame(symbol, date)
  df <- rbind(df, alerts)
  df <- unique(df)

  saveRDS(df, file=alertsFile)
}

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

  df <- tail(alerts, n = n)

  return(df)
}
