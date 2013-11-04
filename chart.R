source("polyReg.R")
source("linReg.R")
source("startProbe.R")
source("orders.R")


chartSymbols <- function(Symbols, startDate="", dateLimit="", xres=2850, yres=1080, dev="",
                         indicators=c("poly_r", "positions", "vol", "sma", "lri"))
{
  require(doMC)
  registerDoMC()
  
  dummy <- foreach(i = 1:length(Symbols)) %dopar%
  { 
    SymbolName <- Symbols[i]
    if(startDate == "")
    {
      st <- seq(as.Date(format(Sys.time(), "%Y-%m-%d")), length=2, by="-1095 days")[2]
    }  
    else
    {
      st <- startDate
    }
    
    if(dateLimit == "")
    {
      ed <- format(Sys.time(), "%Y-%m-%d")
    }
    else
    {
      ed <- dateLimit
    }
    
    dateLimit <- sprintf("%s::%s", st, ed)
    
    Symbol <- get(SymbolName)
    
    if(dev == "png")
    {
      imageName <- sprintf("charts/%s.png", SymbolName)
      png(filename = imageName, width=xres, height=yres)
    }
    
    if("poly_r" %in% indicators)
      polyRegs <- getPolyRegs(SymbolName)
    else
      polyRegs <- NULL
    
    if("sma" %in% indicators)
      sma <- "addSMA(200)"
    else
      sma <- NULL
    
    if("vol" %in% indicators)
      vol <- "addVo()"
    else
      vol <- NULL
    
    if("lri" %in% indicators)
      lri <- getLinRegIndicators(SymbolName)
    else
      lri <- NULL
    
    if("positions" %in% indicators)
      posit <- getOrders(Symbol, SymbolName)
    else
      posit <- NULL
    
    chartSeries(Symbol, name=SymbolName, subset=dateLimit,
                TA=paste(c(polyRegs, sma, vol, posit, lri), collapse="; "))
    
    if(dev == "png")
    {
      dev.off()
    }
  }
}
