source("polyReg.R")
source("linReg.R")
source("startProbe.R")
source("orders.R")
source("meanPrice.R")
source("smaSD.R")

chartSymbols <- function(Symbols, period="", dateLimit="", xres=1900, yres=720, dev="", path="charts", suffix=NULL,
                         Posit=NULL, indicators=c("poly_r", "positions", "vol", "lri", "smaSD", "lriOrders"), timeFrame="daily")
{
  for(i in 1:length(Symbols))
  { 
    SymbolName <- Symbols[i]
    if(period == "")
    {
      st <- seq(as.Date(format(Sys.time(), "%Y-%m-%d")), length=2, by="-2 years")[2]
    }  
    else
    {
      st <- seq(as.Date(format(Sys.time(), "%Y-%m-%d")), length=2, by=paste("-", period, sep = ""))[2]
    }
    
    if(dateLimit == "")
    {
      ed <- format(Sys.time(), "%Y-%m-%d")
    }
    else
    {
      ed <- dateLimit
    }
    
    datePeriod <- sprintf("%s::%s", st, ed)
    
    if(timeFrame == "weekly")
      Symbol <- to.weekly(get(SymbolName))
    else
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
      lriOrders <- getLinRegOrders(SymbolName, get(SymbolName)[sprintf("/%s", ed)], linearRegressionIndicator(SymbolName)[sprintf("/%s", ed)], threshold=0.6)
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
    
    chartSeries(Symbol, name=SymbolName, subset=datePeriod,
                TA=paste(c(polyRegs, smasd, vol, posit, lri, lriOrders, mePrice), collapse="; "))
    
    if(dev == "png")
    {
      dev.off()
      
      imagePath <- sprintf("chart-history/%s", symbol)
      dir.create(imagePath, showWarnings=FALSE)
      file.copy(imageName, sprintf("%s/%s-%s.png", imagePath, ed, SymbolName))
    }
   
    list <- ls(pattern=sprintf("*%s*", SymbolName))
    rm(list = list[list != SymbolName], envir =  .GlobalEnv)
  }
}
