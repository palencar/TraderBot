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
    FilterSymbols <- filterIncomplete(Symbols)
    
    alertas <- filterPolyReg(FilterSymbols, 90, 360, minSigma=-2.0, maxSigma=2.0)
    
    for(i in 1:length(alertas$names))
    {
      imageName <- sprintf("%s-poly-2-sigma.png", alertas[[i]]$name)
      png(filename = imageName, width = 1200, height = 480)
      plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma*2.0)
      dev.off()  
    }
    
    trends <- c("r_up", "r_down")
    alertas <- filterRevert(FilterSymbols, 90, 360, trends)
    
    for(i in 1:length(alertas$names))
    {
      imageName <- sprintf("%s-%s.png", alertas[[i]]$name, alertas[[i]]$trend)
      png(filename = imageName, width = 1200, height = 480)
      plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma*1.0)
      dev.off()
    }
    
    Wallet <- c("CESP6.SA", "CMIG4.SA", "CPFE3.SA", "ELET3.SA", "EQTL3.SA")
    alertas <- filterPolyReg(Wallet, 90, 360)

    for(i in 1:length(alertas$names))
    {
      imageName <- sprintf("%s-wallet.png", alertas[[i]]$name)
      png(filename = imageName, width = 1200, height = 480)
      plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma*1.0)
      dev.off()
    }
    
    fsmState <- "end"
  }
}
