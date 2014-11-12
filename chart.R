source("polyReg.R")
source("linReg.R")
source("startProbe.R")
source("orders.R")
source("meanPrice.R")
source("smaSD.R")

chartSymbols <- function(Symbols, startDate="", dateLimit="", xres=1900, yres=720, dev="", path="charts", suffix=NULL,
                         Posit=NULL, indicators=c("poly_r", "positions", "vol", "sma", "lri", "smaSD", "lriOrders"))
{
  #chartedSymbols <- foreach(i = 1:length(Symbols), .errorhandling="remove") %dopar%
  for(i in 1:length(Symbols))
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

    if("smaSD" %in% indicators)
      smasd <- smaSD(SymbolName, 200)
    else
      smasd <- NULL
    
    if("vol" %in% indicators)
      vol <- "addVo()"
    else
      vol <- NULL
    
    if("lri" %in% indicators)
      lri <- getLinRegIndicators(SymbolName)
    else
      lri <- NULL
    
    if("lriOrders" %in% indicators)
      lriOrders <- getLinRegOrders(SymbolName, get(SymbolName)[sprintf("/%s", ed)], linearRegressionIndicator(SymbolName)[sprintf("/%s", ed)], threshold=1.2)
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
    
    if("meanPrice" %in% indicators)
    {
      mePrice <- getMeanPrice(Symbol, SymbolName)
    }
    else
    {
      mePrice <- NULL
    }
    
    chartSeries(Symbol, name=SymbolName, subset=dateLimit,
                TA=paste(c(polyRegs, sma, smasd, vol, posit, lri, lriOrders, mePrice), collapse="; "))
    
    if(dev == "png")
    {
      dev.off()
    }
   
    list <- ls(pattern=sprintf("*%s*", SymbolName))
    rm(list = list[list != SymbolName], envir =  .GlobalEnv)
  }
}
