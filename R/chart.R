source("R/polyReg.R")
source("R/linReg.R")
source("R/dbInterface.R")
source("R/orders.R")
source("R/meanPrice.R")
source("R/smaSD.R")
source("R/filters.R")

#' @export
chartSymbols <- function(Symbols, period=NULL, dateLimit=NULL, startDate=NULL, endDate=NULL, xres=1900, yres=720, dev="", path="charts", suffix=NULL,
                         Posit=NULL, indicators=c("poly_r", "positions", "vol", "lri", "smaSD", "lriOrders", "meanPrice"), timeFrame="daily", smaPeriod = 400)
{
  for(i in 1:length(Symbols))
  {
    SymbolName <- Symbols[i]

    if(!is.null(endDate))
      ed <- as.Date(endDate)
    else if(is.null(dateLimit))
      ed <- format(Sys.time(), "%Y-%m-%d")
    else
      ed <- dateLimit

    if(!is.null(startDate))
      st <- as.Date(startDate)
    else if(is.null(period))
      st <- seq(as.Date(ed), length=2, by="-5 years")[2]
    else
      st <- seq(as.Date(ed), length=2, by=paste("-", period, sep = ""))[2]

    if(dev == "png")
    {
      if(is.null(suffix) == FALSE)
        imageName <- sprintf("%s/%s-%s.png", path, SymbolName, suffix)
      else
        imageName <- sprintf("%s/%s.png", path, SymbolName)

      dir.create(path, showWarnings=FALSE, recursive = TRUE)
      png(filename = imageName, width=xres, height=yres)
    }

    if("poly_r" %in% indicators)
      polyRegs <- getPolyRegs(SymbolName, endDate=ed)
    else
      polyRegs <- NULL

    Symbol <- base::get(SymbolName)[sprintf("::%s", ed)]

    if("smaSD" %in% indicators)
      smasd <- smaSD(Symbol, smaPeriod)
    else
      smasd <- NULL

    if("vol" %in% indicators)
      vol <- "addVo()"
    else
      vol <- NULL

    if(timeFrame == "weekly")
    {
      Symbol <- to.weekly(Symbol)
    }

    if("lri" %in% indicators)
      lri <- getLinRegIndicators(SymbolName, Symbol, 30)
    else
      lri <- NULL

    if("lriOrders" %in% indicators)
      lriOrders <- getLinRegOrders(SymbolName, Symbol, linearRegressionIndicator(SymbolName, Symbol, n=30))
    else
      lriOrders <- NULL

    if("positions" %in% indicators)
    {
      if(is.null(Posit) == TRUE)
      {
        posit <- getOrders(SymbolName)
      }
      else
      {
        posit <- Posit
      }
    }
    else
    {
      posit <- NULL
    }

    if("meanPrice" %in% indicators)
    {
      mePrice <- getMeanPrice(Symbol, SymbolName)
    }
    else
    {
      mePrice <- NULL
    }

    datePeriod <- sprintf("%s::%s", st, ed)
    taIndicators <- paste(c(polyRegs, smasd, vol, posit, lri, lriOrders, mePrice), collapse="; ")

    chartSeries(Symbol, name=SymbolName, subset=datePeriod, TA=taIndicators)

    if(dev == "png")
    {
      dev.off()

      if(is.null(suffix))
      {
        imagePath <- sprintf("chart-history/%s", SymbolName)
        dir.create(imagePath, showWarnings=FALSE, recursive = TRUE)
        file.copy(imageName, sprintf("%s/%s-%s.png", imagePath, ed, SymbolName))
      }
    }

    list <- ls(pattern=sprintf(".*%s.*", SymbolName))
    rm(list = list[list != SymbolName], envir =  .GlobalEnv)
  }
}

#' @export
chartDaily <- function(symbols, dev="")
{
  chartSymbols(symbols, dev=dev)
}

#' @export
chartWeekly <- function(symbols, dev="")
{
  chartSymbols(Symbols=symbols, period="10 years", timeFrame = "weekly", dev = dev, path = "chart-weekly/")
}

#' @export
chartAlerts <- function()
{
  alertsFile <- "datacache/alerts.rds"
  if(!is.null(alertsFile))
  {
    symbols <- startProbe(alertsFile, FALSE)
    symbols <- filterGap(symbols, lastTradingSession())
    chartDaily(symbols)
    chartWeekly(symbols)
  }
}

#' @export
chartWallet <- function(symbols = NULL, daily = TRUE, weekly = FALSE, dev = "")
{
  symbols <- getWallet()
  if(length(symbols) > 0)
  {
    chartList(symbols, daily = daily, weekly = weekly, dev = dev)
  }
}

#' @export
chartList <- function(symbols = NULL, daily = TRUE, weekly = FALSE, dev = "")
{
  symbols <- startProbe(symbols, FALSE)
  symbols <- filterGap(symbols, lastTradingSession())

  if(daily)
  {
    chartDaily(symbols, dev = dev)
  }

  if(weekly)
  {
    chartWeekly(symbols, dev = dev)
  }
}
