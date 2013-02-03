source("startProbe.R")
source("filters.R")
source("poly-reg.R")
source("plot.R")

args <- c()
args[1] <- as.Date(Sys.Date() - 30)
args[2] <- as.Date(Sys.Date())

args_cmd <- commandArgs(trailingOnly=TRUE)

if(length(args_cmd) == 2)
{
  args <- args_cmd
}

Symbols <- startProbe()

trends <- c("r_up", "r_down")

for(dt in seq(as.Date(args[2]), as.Date(args[1]), by = "-1 day"))
{
  chartDate <- format(as.Date(dt), "%Y-%m-%d")
  
  FilterSymbols <- filterIncomplete(Symbols, dt)
  
  k1 <- 20
  k2 <- 730
  
  strOut <- sprintf("%d ... %d dateLimit=%s", k1, k2, as.Date(dt))
  print(strOut)
  
  for(SymbolName in FilterSymbols)
  {
    alertas <- findRevertCurves(c(SymbolName), minDays=k1, maxDays=k2, trend=trends, dateLimit=dt)
    if(length(alertas) >= 1)
    {
      for(i in 1:length(alertas))
      {
        objectName <- sprintf("teste/%s-%s_%d_%s.Robj", chartDate, alertas[[i]]$name,
                              alertas[[i]]$period, alertas[[i]]$trend)
        print(objectName)
        dput(alertas[[i]], objectName)
        
        imageName <- sprintf("teste/%s-%s_%d_%s.png", chartDate, alertas[[i]]$name,
                             alertas[[i]]$period, alertas[[i]]$trend)
        png(filename = imageName, width = 1900, height = 1080)
        plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma, alertas[[i]]$interval)
        dev.off()
      }
    }
  }
}