source("startProbe.R")
source("filters.R")
source("poly-reg.R")
source("plot.R")

fsmState <- "startProbe"

#args <- commandArgs(trailingOnly=TRUE)
#print(args)

while(fsmState != "end")
{
  if(fsmState == "startProbe")
  {
    print("startProbe")
    Symbols <- startProbe()
    fsmState <- "loadFilters"
  }
  else if(fsmState == "loadFilters")
  {
    print("loadFilters")
    loadFilters(Symbols, c("polyreg"))
    fsmState <- "applyFilters"
  }
  else if(fsmState == "applyFilters")
  {
    print("applyFilters")
    
    #chartDir <- format(Sys.time(), "%Y-%m-%d")
    chartDir <- format(Sys.time(), "%Y-%m-%d_%H:%M:%S")
    dir.create(chartDir, showWarnings = FALSE)
    
    print("Wallet")
    Wallet <- c("CESP6.SA", "CMIG4.SA", "CPFE3.SA", "ELET3.SA", "EQTL3.SA", "LIGT3.SA", "ELPL4.SA", "PETR4.SA")
    alertas <- filterPolyReg(Wallet, 90, 150)
    if(length(alertas$names) >= 1)
    {
      for(i in 1:length(alertas$names))
      {
        objectName <- sprintf("%s/%s_wallet.Robj", chartDir, alertas[[i]]$name)
        print(objectName)
        dput(alertas[[i]], objectName)
        
        imageName <- sprintf("%s/%s_wallet.png", chartDir, alertas[[i]]$name)
        print(imageName)
        png(filename = imageName, width = 1900, height = 1080)
        plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma*1.0)
        dev.off()
      }
    }
    
    alreadyPlot <- Wallet
    filterSymbols <- filterIncomplete(Symbols)
    FilterSymbols <- filterSymbols[!filterSymbols %in% alreadyPlot]
    trends <- c("r_up", "r_down")
    
    k <- 30
    for(j in seq(1, 3, by=1))
    {
      k <- 2*k   #60-120, 120-240, 240-480
      
      strOut <- sprintf("filterRevert r_up r_down %d %d", k, 4*k)
      print(strOut)
      
      alertas <- filterRevert(FilterSymbols, k, 4*k, trends, 4)
      
      if(length(alertas$names) >= 1)
      {
        for(i in 1:length(alertas$names))
        {
          #imageName <- sprintf("%s/%s_%s.png", chartDir, alertas[[i]]$name, alertas[[i]]$trend)
          #png(filename = imageName, width = 1200, height = 480)
          #plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma*1.0)
          #dev.off()
          
          objectName <- sprintf("%s/%s_%d_%d_%s.Robj", chartDir, alertas[[i]]$name,
                                k, 2*k, alertas[[i]]$trend)
          print(objectName)
          dput(alertas[[i]], objectName)
          
          imageName <- sprintf("%s/%s_%d_%d_%s.png", chartDir, alertas[[i]]$name,
                               k, 2*k, alertas[[i]]$trend)
          png(filename = imageName, width = 1900, height = 1080)
          plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma)
          dev.off()
        }
      }
    }
    
    fsmState <- "startProbe"
  }
}
