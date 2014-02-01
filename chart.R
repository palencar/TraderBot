source("polyReg.R")
source("linReg.R")
source("startProbe.R")
source("orders.R")


chartSymbols <- function(Symbols, startDate="", dateLimit="", xres=1900, yres=720, dev="", path="charts", suffix=NULL,
                         Posit=NULL, indicators=c("poly_r", "positions", "vol", "sma", "lri", "lriOrders"))
{
  chartedSymbols <- foreach(i = 1:length(Symbols), .errorhandling="remove") %dopar%
  #for(i in 1:length(Symbols))
  { 
    SymbolName <- Symbols[i]
    if(startDate == "")
    {
      st <- seq(as.Date(format(Sys.time(), "%Y-%m-%d")), length=2, by="-730 days")[2]
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
      if(is.null(suffix) == FALSE)      
        imageName <- sprintf("%s/%s-%s.png", path, SymbolName, suffix)
      else
        imageName <- sprintf("%s/%s.png", path, SymbolName)
      
      png(filename = imageName, width=xres, height=yres)
    }
    
    if("poly_r" %in% indicators)
      polyRegs <- getPolyRegs(SymbolName, endDate=ed)
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
    
    if("lriOrders" %in% indicators)
      lriOrders <- getLinRegOrders(get(SymbolName)[sprintf("/%s", ed)], linearRegressionIndicator(SymbolName)[sprintf("/%s", ed)], threshold=1.2)
    else
      lriOrders <- NULL
    
    if("positions" %in% indicators)
    {
      if(is.null(Posit) == TRUE)
      {
        posit <- getOrders(Symbol, SymbolName)
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
    
    chartSeries(Symbol, name=SymbolName, subset=dateLimit,
                TA=paste(c(polyRegs, sma, vol, posit, lri, lriOrders), collapse="; "))
        
    if(dev == "png")
    {
      dev.off()
    }
    
    #SymbolName
  }
  #return(chartedSymbols)
}
