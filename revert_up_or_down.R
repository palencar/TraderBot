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
    for(trend in trends)
    {
      alertas <- findRevertCurves(c(SymbolName), minDays=k1, maxDays=k2, trend=trend, dateLimit=dt)
      if(length(alertas) >= 1)
      {
        if(length(alertas) >= 1)
        {
          best <- 1
          
          for(i in 1:length(alertas))
          {
            if(alertas[[i]]$sigma < alertas[[best]]$sigma)
            {
              best <- i
            }
          }
          objectName <- sprintf("teste/%s-%s_%d_%s.Robj", chartDate, alertas[[best]]$name,
                                alertas[[best]]$period, alertas[[best]]$trend)
          print(objectName)
          dput(alertas[[best]], objectName)
          
          imageName <- sprintf("teste/%s-%s_%d_%s.png", chartDate, alertas[[best]]$name,
                               alertas[[best]]$period, alertas[[best]]$trend)
          png(filename = imageName, width = 1900, height = 1080)
          plotPolyReg(alertas[[best]]$name, alertas[[best]]$regression, alertas[[best]]$sigma, alertas[[best]]$interval)
          dev.off()
        }
      }
    }
  }
}