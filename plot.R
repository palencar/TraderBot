source('poly-reg.R')
source('startProbe.R')

plotSymbol <- function(SymbolName, minDays=90, maxDays=150, dateLimit="", startDate="")
{
  if(exists(Symbols) == FALSE)
  {
    Symbols <- startProbe()
  }
  
  reg <- findBestCurve(SymbolName, minDays, maxDays, dateLimit="")
  
  plotPolyReg(reg$name, reg$regression, reg$sigma, startDate=startDate, dateLimit=dateLimit)
}

plotObject <- function(FileName, xres=1900, yres=1080, dev="", startDate="")
{
  object <- dget(FileName)
  
  if(dev == "png")
  {
    imageName <- gsub(".Robj", ".png", x=FileName)
    png(filename = imageName, width=xres, height=yres)
    plotPolyReg(object$name, object$regression, object$sigma, startDate=startDate)
    dev.off()
  }
  else
  {
    plotPolyReg(object$name, object$regression, object$sigma, startDate=startDate)
  }
}

plotObjectSet <- function(FileName, xres=1900, yres=1080, dev="", startDate="")
{
  #objects <- dget(FileName)
  objects <- readRDS(file=FileName)
  
  if(dev == "png")
  {
    for(i in 1:length(1:length(objects$names)))
    {
      object <- objects[[i]]
      suffix <- sprintf("_%d.png", object$period)
      imageName <- gsub(".rds", suffix, x=FileName)
      png(filename = imageName, width=xres, height=yres)
      plotPolyReg(object$name, object$regression, object$sigma, startDate=startDate)
      dev.off()
    }
  }
  else
  {
    for(i in 1:length(objects$names))
    {
      object <- objects[[i]]
      plotPolyReg(object$name, object$regression, object$sigma, startDate=startDate)
    }
  }
}

plotPolyReg <- function(SymbolName, polyReg, sigma, dateLimit="", startDate="")
{
  #buscar datas das ordens executadas
  
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