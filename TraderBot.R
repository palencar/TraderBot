source("startProbe.R")
source("filters.R")
source("polyReg.R")
source("chart.R")
source("Stream.R")
source("Backtest.R")


args <- commandArgs(trailingOnly=TRUE)
print(args)

stream = FALSE
Symbols = NULL
alerts = NULL

if(length(args) > 0)
{
  if(args[1] == "stream")
  {
    stream = TRUE
    
    if(length(args) > 1)
    {
      Symbols <- tail(args, n=(length(args)-1))
    }
    
    computeStream(Symbols)
  }
  else if(args[1] == "compute")
  {
    if(length(args) >= 3)
    {
      startDate <- args[2]
      endDate <- args[3]
      if(length(args) > 3)
      {
        Symbols <- tail(args, n=(length(args)-3))
      }
    }
    else if(length(args) == 2)
    {
      startDate <- endDate <- args[2]
    }
    else
    {
      startDate <- endDate <- Sys.Date()
    }

    Symbols <- startProbe(symbolNames = Symbols, minAge=200, update=FALSE)
    
    Symbols <- filterVolume(Symbols)
    Symbols <- filterIncomplete(Symbols)
    
    computeBacktest(Symbols, startDate, endDate, TRUE)
  }
}


