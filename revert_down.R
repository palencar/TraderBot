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

#if(exists(Symbols) == FALSE)
#{
#  Symbols <- startProbe()
#}

trends <- c("r_down")

for(dt in seq(as.Date(args[2]), as.Date(args[1]), by = "-1 day"))
{
  chartDate <- format(as.Date(dt), "%Y-%m-%d")
  
  FilterSymbols <- filterIncomplete(Symbols, dt)
  
  k <- 30
  
  for(j in seq(1, 3, by=1))
  {
    k <- 2*k   #60-120, 120-240, 240-480
    
    strOut <- sprintf("%d ... %d dateLimit=%s", k, 2*k, as.Date(dt))
    print(strOut)
    
    alertas <- filterRevert(FilterSymbols, minDays=k, maxDays=2*k, trend=trends, period=4, dateLimit=dt) 
    if(length(alertas$names) >= 1)
    {
      for(i in 1:length(alertas$names))
      {
        objectName <- sprintf("teste/%s-%s_%d_%d_%s.Robj", chartDate, alertas[[i]]$name,
                              k, 2*k, alertas[[i]]$trend)
        print(objectName)
        dput(alertas[[i]], objectName)
        
        imageName <- sprintf("teste/%s-%s_%d_%d_%s.png", chartDate, alertas[[i]]$name,
                             k, 2*k, alertas[[i]]$trend)
        png(filename = imageName, width = 1900, height = 1080)
        plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma)
        dev.off()
      }
    }
  }
}