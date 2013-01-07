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
    
    Wallet <- c("CESP6.SA", "CMIG4.SA", "CPFE3.SA", "ELET3.SA", "EQTL3.SA", "LIGT3.SA", "ELPL4.SA")
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
    
    filterSymbols <- filterIncomplete(Symbols)
    
    alreadyPlot <- Wallet
    FilterSymbols <- filterSymbols[!filterSymbols %in% alreadyPlot]
    
    alertas <- filterPolyReg(FilterSymbols, 90, 360, minSigma=-2.0)
    
    if(length(alertas$names) >= 1)
    {
      for(i in 1:length(alertas$names))
      {
        imageName <- sprintf("%s/%s-2sigma.png", chartDir, alertas[[i]]$name)
        png(filename = imageName, width = 1200, height = 480)
        plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma*2.0)
        dev.off()  
      }
    }
    
    alreadyPlot <- c(alreadyPlot, alertas$names)
    FilterSymbols <- filterSymbols[!filterSymbols %in% alreadyPlot]
    
    alertas <- filterPolyReg(FilterSymbols, 90, 360, minSigma=-1.0)
    trends <- c("r_up")
    alertas <- filterRevert(alertas$names, 90, 360, trends, 30)
    
    if(length(alertas$names) >= 1)
    {
      for(i in 1:length(alertas$names))
      {
        imageName <- sprintf("%s/%s-1sigma_%s.png", chartDir, alertas[[i]]$name, alertas[[i]]$trend)
        png(filename = imageName, width = 1200, height = 480)
        plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma*1.0)
        dev.off()
      }
    }
    
    alreadyPlot <- c(alreadyPlot, alertas$names)
    FilterSymbols <- filterSymbols[!filterSymbols %in% alreadyPlot]
    trends <- c("r_up")
    alertas <- filterRevert(FilterSymbols, 90, 360, trends, 30)
    
    if(length(alertas$names) >= 1)
    {
      for(i in 1:length(alertas$names))
      {
        imageName <- sprintf("%s/%s_%s.png", chartDir, alertas[[i]]$name, alertas[[i]]$trend)
        png(filename = imageName, width = 1200, height = 480)
        plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma*1.0)
        dev.off()
      }
    }
    
    fsmState <- "end"
  }
}
