source("startProbe.R")
source("filters.R")
source("polyReg.R")
source("chart.R")


args <- commandArgs(trailingOnly = TRUE)

if(length(args) < 3)
{
  quit()
}

symbolName <- args[1]
startDate <- args[2]
endDate <- args[3]

window <- 730

AllSymbols <- startProbe(update = FALSE)


for(day in as.Date(startDate):as.Date(endDate))
{
  startChart <- sprintf("%s", as.Date(day-window))
  endChart <- sprintf("%s", as.Date(day))
  
  alertR <- computeRegressions(symbolName, endChart, endChart)
  
  alertL <- filterLRI(get(symbolName), linearRegressionIndicator(symbolName)[sprintf("/%s", endChart)], threshold=1.2)
  
  if(is.null(alertR) == FALSE || is.null(alertL) == FALSE)
  {
    #if(alertL (r_up) && alertL < SMA(200)")
    #{
    #  add virtual position (buy)
    #}
    #if((alertL (r_dow) && alertL > SMA(200)) || (gain(last buy) > 10%))
    #{
    #  close all virtual positions
    #}
    
    imagePath <- sprintf("charts/%s", symbolName)
    dir.create(imagePath, showWarnings=FALSE)
    
    chartSymbols(symbolName, startDate=startChart, dateLimit=endChart, dev="png", path=imagePath, suffix=day)
  }
}