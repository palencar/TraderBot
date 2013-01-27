source("startProbe.R")
source("filters.R")
source("poly-reg.R")
source("plot.R")

args <- c()
args[1] <- as.Date(Sys.Date() - 30)
args[2] <- as.Date(Sys.Date())

args_cmd <- commandArgs(trailingOnly=TRUE)

if(length(args_cmd) >= 2)
{
  args <- args_cmd

  if(length(args_cmd) >= 3)  a <- as.double(args[3])
  else                       a <- -0.5
  
  if(length(args_cmd) >= 4)  b <- as.double(args[4])
  else                       b <- 2.0
  
  if(length(args_cmd) >= 5)  inc <- as.double(args[5])
  else                       inc <- -0.5
}

if(exists(Symbols) == FALSE)
{
  Symbols <- startProbe()
}

trends <- c("r_up")

for(dt in seq(as.Date(args[2]), as.Date(args[1]), by = "-1 day"))
{
  chartDate <- format(as.Date(dt), "%Y-%m-%d")
  
  FilterSymbols <- filterIncomplete(Symbols, dt)

  for(k in seq(60, 90, by=30))
  {
    for(j in seq(-0.5, -2.0, by=-0.5))
    {
      strOut <- sprintf("%d ... %d minSigma=%.2f dateLimit=%s", k, 4*k, j, as.Date(dt))
      print(strOut)

      alertas <- filterPolyReg(FilterSymbols, k, 4*k, j, dateLimit=dt)

      if(length(alertas$names) >= 1)
      {
        alertas <- filterRevert(alertas$names, k, 4*k, trends, period=4, dateLimit=dt) 
        if(length(alertas$names) >= 1)
        {
          for(i in 1:length(alertas$names))
          {
            objectName <- sprintf("teste/%s-%s%.2fsigma_%d_%d_%s.Robj", chartDate, alertas[[i]]$name,
                                  j, k, 4*k, alertas[[i]]$trend)
            print(objectName)
            dput(alertas[[i]], objectName)
            
            imageName <- sprintf("teste/%s-%s%.2fsigma_%d_%d_%s.png", chartDate, alertas[[i]]$name,
                                 j, k, 4*k, alertas[[i]]$trend)
            png(filename = imageName, width = 1900, height = 1080)
            plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma*(-j))
            dev.off()
          }
        }
      }
      
      if(length(alertas$names) == 0)
      {
        break
      }
    }
  }
}