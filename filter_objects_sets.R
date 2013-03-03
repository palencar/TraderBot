source("startProbe.R")
source("filters.R")
source("poly-reg.R")
source("plot.R")
source("utils.R")

args <- c()
args[1] <- as.Date(Sys.Date() - 30)
args[2] <- as.Date(Sys.Date())

args_cmd <- commandArgs(trailingOnly=TRUE)

if(length(args_cmd) == 2)
{
  args <- args_cmd
}

Symbols <- startProbe()

k1 <- 10
k2 <- 730
trends <- c("r_up")

for(dt in seq(as.Date(args[2]), as.Date(args[1]), by = "-1 day"))
{
  chartDate <- sprintf("%s", as.Date(dt))
  
  filterSymbols <- filterIncomplete(Symbols)
  
  for(symbol in filterSymbols)
  {
    if(length(get(symbol)[chartDate]) == 0)
      next
    
    strOut <- sprintf("filterRevert %s r_up %d %d %s", symbol, k1, k2, chartDate)
    print(strOut)
    
    objectName <- sprintf("backtest/%s-%s_%d_%d.rds", chartDate, symbol, k1, k2)
    alertas <- readRDS(file=objectName)
    
    #if(length(alertas) > 0)
    #{
    #  objectName <- sprintf("backtest/%s-%s_%d_%d.rds", chartDate, symbol, k1, k2)
    #  
    #  #dput(alertas, objectName)
    #  saveRDS(alertas, file=objectName)
    #}
    if(length(alertas) == 0)
    {
      next
    }
    
    alertas <- turnPoints(alertas)
    
    if(length(alertas) > 0)
    {
      objectName <- sprintf("backtest/%s-%s_%d_%d_turnpoints.rds", chartDate, symbol, k1, k2)
      
      #dput(alertas, objectName)
      saveRDS(alertas, file=objectName)
    }
    if(length(alertas) == 0)
    {
      next
    }
    
    alertas <- filterRevert(alertas, trends, 3)
    
    if(length(alertas) > 0)
    {
      objectName <- sprintf("backtest/%s-%s_%d_%d_turnpoints_r_up.rds", chartDate, symbol, k1, k2)
      
      #dput(alertas, objectName)
      saveRDS(alertas, file=objectName)
    }
    if(length(alertas) == 0)
    {
      next
    }
    
    #if(length(alertas$names) >= 1)
    #{
    #  for(i in 1:length(alertas$names))
    #  {
    #    objectName <- sprintf("backtest/%s-%s_%d_%d_%s.Robj", chartDate, alertas[[i]]$name,
    #                          k1, k2, alertas[[i]]$trend)
    #    print(objectName)
    #    dput(alertas[[i]], objectName)
    #    
    #    imageName <- sprintf("backtest/%s-%s_%d_%d_%s.png", chartDate, alertas[[i]]$name,
    #                         k1, k2, alertas[[i]]$trend)
    #    png(filename = imageName, width = 1900, height = 1080)
    #    plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma)
    #    dev.off()
    #  }
    #}
  }
}