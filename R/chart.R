source("R/linReg.R")
source("R/dbInterface.R")
source("R/orders.R")
source("R/meanPrice.R")
source("R/smaSD.R")
source("R/filters.R")

#' @export
chartSymbols <- function(Symbols, period=730, startDate=NULL, endDate=Sys.time(), xres=1900, yres=720, dev="", path="charts", suffix=NULL,
                         mode="operation", indicators=c("positions", "alerts", "vol", "smaSD", "meanPrice"), timeFrame, smaPeriod = 400, lriPeriod = 30)
{
  for(i in 1:length(Symbols))
  {
    SymbolName <- Symbols[i]

    Symbol <- base::get(SymbolName)[sprintf("::%s", endDate)]

    if(!is.null(startDate))
      st <- startDate
    else if(!is.null(period))
      st <- index(first(tail(Symbol, period)))

    if(dev == "png")
    {
      chartName <- SymbolName

      if(is.null(suffix) == FALSE)
        imageName <- sprintf("%s/%s-%s.png", path, chartName, suffix)
      else
        imageName <- sprintf("%s/%s.png", path, chartName)

      dir.create(path, showWarnings=FALSE, recursive = TRUE)
      png(filename = imageName, width=xres, height=yres)
    }

    smasd <- NULL

    if("smaSD" %in% indicators)
      smasd <- smaSD(Symbol, smaPeriod)

    vol <- NULL

    if("vol" %in% indicators)
      vol <- "addVo()"

    posit <- NULL

    if("positions" %in% indicators)
      posit <- getOrders(SymbolName, endDate, mode)

    alerts <- NULL

    if("alerts" %in% indicators)
      alerts <- getAlertSignals(unlist(strsplit(SymbolName, "[._]"))[1], timeFrame)

    mePrice <- NULL

    if("meanPrice" %in% indicators)
      mePrice <- getMeanPrice(Symbol, SymbolName)

    datePeriod <- sprintf("%s::%s", st, endDate)
    taIndicators <- paste(c(smasd, vol, posit, alerts, mePrice), collapse="; ")

    if(timeFrame == "1W")
      Symbol = to.weekly(Symbol)

    chartSeries(Symbol, name=SymbolName, subset=datePeriod, TA=taIndicators)

    if(dev == "png")
      dev.off()

    list <- ls(pattern=sprintf(".*%s.*", SymbolName))
    rm(list = list[list != SymbolName], envir =  .GlobalEnv)
  }
}
