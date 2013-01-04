source("startProbe.R")
source("filters.R")
source("poly-reg.R")
source("plot.R")

fsmState <- "startProbe"

while(fsmState != "end")
{
  if(fsmState == "startProbe")
  {
    Symbols <- startProbe()
    fsmState <- "loadFilters"
  }
  else if(fsmState == "loadFilters")
  {
    loadFilters(Symbols, c("polyreg"))
    fsmState <- "applyFilters"
  }
  else if(fsmState == "applyFilters")
  {
    chartDir <- format(Sys.time(), "%Y-%m-%d")
    dir.create(chartDir, showWarnings = FALSE)
    
    FilterSymbols <- filterIncomplete(Symbols)
    
    alertas <- filterPolyReg(FilterSymbols, 90, 360, minSigma=-2.0)
    
    for(i in 1:length(alertas$names))
    {
      imageName <- sprintf("%s/%s-2sigma.png", chartDir, alertas[[i]]$name)
      png(filename = imageName, width = 1200, height = 480)
      plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma*2.0)
      dev.off()  
    }
    
    alertas <- filterPolyReg(FilterSymbols, 90, 360, minSigma=-1.0)
    trends <- c("r_up")
    alertas <- filterRevert(alertas$names, 90, 360, trends, 30)
    
    for(i in 1:length(alertas$names))
    {
      imageName <- sprintf("%s/%s-1sigma_%s.png", chartDir, alertas[[i]]$name, alertas[[i]]$trend)
      png(filename = imageName, width = 1200, height = 480)
      plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma*1.0)
      dev.off()
    }
    
    trends <- c("r_up")
    alertas <- filterRevert(FilterSymbols, 90, 360, trends, 30)
    
    for(i in 1:length(alertas$names))
    {
      imageName <- sprintf("%s/%s_%s.png", chartDir, alertas[[i]]$name, alertas[[i]]$trend)
      png(filename = imageName, width = 1200, height = 480)
      plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma*1.0)
      dev.off()
    }
    #args <- commandArgs(trailingOnly=TRUE)
    #print(args)

    Wallet <- c("CESP6.SA", "CMIG4.SA", "CPFE3.SA", "ELET3.SA", "EQTL3.SA", "LIGT3.SA")
    alertas <- filterPolyReg(Wallet, 90, 150)
    if(length(alertas$names) >= 1)
    {
      for(i in 1:length(alertas$names))
      {
        imageName <- sprintf("%s/%s_wallet.png", chartDir, alertas[[i]]$name)
        png(filename = imageName, width = 1200, height = 480)
        plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma*1.0)
        dev.off()
      }
    }
    
    #trends <- c("r_down")
    #alertas <- filterRevert(Wallet, 90, 150, trends, 20)
    #
    #if(length(alertas$names) >= 1)
    #{
    #  for(i in 1:length(alertas$names))
    #  {
    #    imageName <- sprintf("%s/%s-wallet.png", chartDir, alertas[[i]]$name)
    #    png(filename = imageName, width = 1200, height = 480)
    #    plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma*1.0)
    #    dev.off()
    #  }
    #}
    
    fsmState <- "end"
  }
}
