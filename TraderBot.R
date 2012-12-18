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
    alertas <- filterPolyReg(Symbols, 100, 400, 2)
    for(i in 1:length(alertas))
    {
      imageName <- sprintf("%s.png", alertas[[i]]$name)
      png(filename = imageName, width = 1200, height = 480)
      plotPolyReg(alertas[[i]]$name, alertas[[i]]$regression, alertas[[i]]$sigma*2.0)
      dev.off()
    }
    fsmState <- "end"
  }
}