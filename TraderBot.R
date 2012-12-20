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
    
    alertas <- filterPolyReg(FilterSymbols, 100, 400, 1.0)
    
    for(i in 1:length(alertas))
    {
      imageName <- sprintf("%s-poly-1-sigma.png", alertas[[i]]$name)
      png(filename = imageName, width = 1200, height = 480)
      plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma*1.0)
      dev.off()
    }
    
    alertas <- filterPolyReg(FilterSymbols, 100, 400, 2.0)
    
    for(i in 1:length(alertas))
    {
      imageName <- sprintf("%s-poly-2-sigma.png", alertas[[i]]$name)
      png(filename = imageName, width = 1200, height = 480)
      plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma*2.0)
      dev.off()
    }
    
    alertas <- filterRevert(FilterSymbols, 100, 400, trend="r_up")
    
    for(i in 1:length(alertas))
    {
      imageName <- sprintf("%s-r_up.png", alertas[[i]]$name)
      png(filename = imageName, width = 1200, height = 480)
      plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma*1.0)
      dev.off()
    }
    
    alertas <- filterRevert(FilterSymbols, 100, 400, trend="r_down")
    
    for(i in 1:length(alertas))
    {
      imageName <- sprintf("%s-r_down.png", alertas[[i]]$name)
      png(filename = imageName, width = 1200, height = 480)
      plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma*1.0)
      dev.off()
    }
    
    alertas <- filterRevert(FilterSymbols, 100, 400, trend="up")
    
    for(i in 1:length(alertas))
    {
      imageName <- sprintf("%s-up.png", alertas[[i]]$name)
      png(filename = imageName, width = 1200, height = 480)
      plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma*1.0)
      dev.off()
    }
    
    alertas <- filterRevert(FilterSymbols, 100, 400, trend="down")
    
    for(i in 1:length(alertas))
    {
      imageName <- sprintf("%s-down.png", alertas[[i]]$name)
      png(filename = imageName, width = 1200, height = 480)
      plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma*1.0)
      dev.off()
    }
    
    fsmState <- "end"
  }
}
