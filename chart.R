source("polyReg.R")
source("linReg.R")
source("dbInterface.R")
source("orders.R")
source("meanPrice.R")
source("smaSD.R")

chartSymbols <- function(Symbols, period=NULL, dateLimit=NULL, xres=1900, yres=720, dev="", path="charts", suffix=NULL,
                         Posit=NULL, indicators=c("poly_r", "positions", "vol", "lri", "smaSD", "lriOrders", "meanPrice"), timeFrame="daily", smaPeriod = 200)
{
  for(i in 1:length(Symbols))
  { 
    SymbolName <- Symbols[i]

    if(is.null(dateLimit))
    {
      ed <- format(Sys.time(), "%Y-%m-%d")
    }
    else
    {
      ed <- dateLimit
    }

    if(is.null(period))
    {
      st <- seq(as.Date(ed), length=2, by="-5 years")[2]
    }  
    else
    {
      st <- seq(as.Date(ed), length=2, by=paste("-", period, sep = ""))[2]
    }

    if(dev == "png")
    {
      if(is.null(suffix) == FALSE)      
        imageName <- sprintf("%s/%s-%s.png", path, SymbolName, suffix)
      else
        imageName <- sprintf("%s/%s.png", path, SymbolName)
      
      dir.create(path, showWarnings=FALSE)
      png(filename = imageName, width=xres, height=yres)
    }
    
    if("poly_r" %in% indicators)
      polyRegs <- getPolyRegs(SymbolName, endDate=ed)
    else
      polyRegs <- NULL
    
    Symbol <- get(SymbolName)[sprintf("::%s", ed)]
    
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
    
    chartSeries(Symbol, name=SymbolName, subset=datePeriod,
                TA=paste(c(polyRegs, smasd, vol, posit, lri, lriOrders, mePrice), collapse="; "))
    
    if(dev == "png")
    {
      dev.off()
      
      dirName <- "chart-history"
      
      if(!is.null(suffix))
      {
        dir.create(dirName, showWarnings=FALSE)
        dirName <- sprintf("%s/%s", path, suffix)
      }
      
      imagePath <- sprintf("%s/%s", dirName, SymbolName)
      dir.create(dirName, showWarnings=FALSE)
      dir.create(imagePath, showWarnings=FALSE)
      file.copy(imageName, sprintf("%s/%s-%s.png", imagePath, ed, SymbolName))
    }
   
    list <- ls(pattern=sprintf("*%s*", SymbolName))
    rm(list = list[list != SymbolName], envir =  .GlobalEnv)
  }
}
