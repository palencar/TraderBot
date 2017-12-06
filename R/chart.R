source("R/linReg.R")
source("R/dbInterface.R")
source("R/orders.R")
source("R/meanPrice.R")
source("R/smaSD.R")
source("R/filters.R")

#' @export
chartSymbols <- function(Symbols, period=730, dateLimit=NULL, startDate=NULL, endDate=NULL, xres=1900, yres=720, dev="", path="charts", suffix=NULL,
                         Posit=NULL, indicators=c("positions", "vol", "lri", "smaSD", "lriOrders", "meanPrice"), timeFrame="daily", smaPeriod = 400)
{
  for(i in 1:length(Symbols))
  {
    SymbolName <- Symbols[i]

    if(!is.null(endDate))
      ed <- endDate
    else if(is.null(dateLimit))
      ed <- Sys.time()
    else
      ed <- dateLimit

    Symbol <- base::get(SymbolName)[sprintf("::%s", ed)]

    if(!is.null(startDate))
      st <- startDate
    else if(!is.null(period))
      st <- index(first(tail(Symbol, period)))

    if(dev == "png")
    {
      chartName <- SymbolName

      tf <- unlist(strsplit(SymbolName, "[._]"))[2]

      if(is.na(tf) && timeFrame == "daily")
        chartName <- paste0(SymbolName, ".1D")

      if(is.na(tf) && timeFrame == "weekly")
        chartName <- paste0(SymbolName, ".1W")

      if(is.null(suffix) == FALSE)
        imageName <- sprintf("%s/%s-%s.png", path, chartName, suffix)
      else
        imageName <- sprintf("%s/%s.png", path, chartName)

      dir.create(path, showWarnings=FALSE, recursive = TRUE)
      png(filename = imageName, width=xres, height=yres)
    }

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
    taIndicators <- paste(c(smasd, vol, posit, lri, lriOrders, mePrice), collapse="; ")

    chartSeries(Symbol, name=SymbolName, subset=datePeriod, TA=taIndicators)

    if(dev == "png")
    {
      dev.off()
    }

    list <- ls(pattern=sprintf(".*%s.*", SymbolName))
    rm(list = list[list != SymbolName], envir =  .GlobalEnv)
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
  symbols <- getSymbolsDaily(symbols, adjust = c("split", "dividend"))

  if(daily)
  {
    chartDaily(symbols, dev = dev)
  }

  if(weekly)
  {
    chartWeekly(symbols, dev = dev)
  }
}
