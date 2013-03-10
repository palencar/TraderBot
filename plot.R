source('poly-reg.R')
source('startProbe.R')

plotSymbol <- function(SymbolName, minDays=90, maxDays=150, dateLimit="", startDate="")
{
  if(exists(Symbols) == FALSE)
  {
    Symbols <- startProbe()
  }
  
  reg <- findBestCurve(SymbolName, minDays, maxDays, dateLimit="")
  
  plotPolyReg(reg$name, reg$regression, reg$sigma, startDate=startDate, dateLimit=dateLimit, showPositions=TRUE)
}

plotObject <- function(FileName, xres=1900, yres=1080, dev="", startDate="")
{
  object <- dget(FileName)
  
  if(dev == "png")
  {
    imageName <- gsub(".Robj", ".png", x=FileName)
    png(filename = imageName, width=xres, height=yres)
    plotPolyReg(object$name, object$regression, object$sigma, startDate=startDate, showPositions=TRUE)
    dev.off()
  }
  else
  {
    plotPolyReg(object$name, object$regression, object$sigma, startDate=startDate, showPositions=TRUE)
  }
}

plotObjectSet <- function(FileNames, xres=1900, yres=1080, dev="", startDate="")
{
  for(name in FileNames)
  {
    objects <- readRDS(file=FileName)
    
    if(dev == "png")
    {
      for(i in 1:length(1:length(objects$names)))
      {
        object <- objects[[i]]
        suffix <- sprintf("_%d.png", object$period)
        imageName <- gsub(".rds", suffix, x=FileName)
        png(filename = imageName, width=xres, height=yres)
        plotPolyReg(object$name, object$regression, object$sigma, startDate=startDate, showPositions=TRUE)
        dev.off()
      }
    }
    else
    {
      for(i in 1:length(objects$names))
      {
        object <- objects[[i]]
        plotPolyReg(object$name, object$regression, object$sigma, startDate=startDate, showPositions=TRUE)
      }
    }
  }
}

plotPolyReg <- function(SymbolName, polyReg, sigma, dateLimit="", startDate="", showPositions=FALSE)
{
  if(startDate == "")
  {
    st <- format(as.Date(index(first(polyReg))) - 2*length(polyReg), "%Y-%m-%d")
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
  
  Symbol <- get(SymbolName)[dateLimit]
  
  chartSeries(Symbol, name=SymbolName)
  plot(addTA(polyReg, on=1, col=3))
  plot(addTA(polyReg+sigma, on=1, col=7))
  plot(addTA(polyReg-sigma, on=1, col=7))
  
  if(showPositions == TRUE)
  {
    addOrders(Symbol, SymbolName)
  }
}

plotPolyRegs <- function(Objects, dateLimit="", startDate="", xres=1900, yres=1080, dev="")
{ 
  object <- Objects[[1]]
  SymbolName <- object$name
  st <-  format(as.Date(index(first(object$regression))))
  
  for(object in Objects)
  {
    polyReg <- object$regression
    
    if(startDate == "")
    {
      stNew <- format(as.Date(index(first(polyReg))) - 2*length(polyReg), "%Y-%m-%d")
    }
    else
    {
      stNew <- startDate
    }
    
    if(stNew < st)
    {
      st <- stNew
    }
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
  
  Symbol <- get(SymbolName)[dateLimit]
  
  if(dev == "png")
  {
    imageName <- sprintf("plots/%s.png", SymbolName)
    png(filename = imageName, width=xres, height=yres)
  }
  
  chartSeries(Symbol, name=SymbolName)
  
  for(object in Objects)
  {
    polyReg <- object$regression
    sigma <- object$sigma
    
    plot(addTA(polyReg, on=1, col=3))
    plot(addTA(polyReg+sigma, on=1, col=7))
    plot(addTA(polyReg-sigma, on=1, col=7))
  }
  
  if(dev == "png")
  {
    dev.off()
  }
}

plotLinearReg <- function (SymbolName, linReg, sigma, dateLimit="", startDate="")
{
  #buscar datas das ordens executadas
  
  if(startDate == "")
  {
    st <- format(as.Date(index(first(linReg))) - 2*length(linReg), "%Y-%m-%d")
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
  
  Symbol <- get(SymbolName)[dateLimit]
  
  chartSeries(Symbol, name=SymbolName)
  plot(addTA(linReg, on=1, col=3))
  plot(addTA(linReg+sigma, on=1, col=7))
  plot(addTA(linReg-sigma, on=1, col=7))
}
