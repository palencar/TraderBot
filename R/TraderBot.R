source("R/dbInterface.R")
source("R/filters.R")
source("R/polyReg.R")
source("R/chart.R")
source("R/stream.R")
source("R/backtest.R")

dir.create("data", showWarnings=FALSE)

args <- commandArgs(trailingOnly=TRUE)
print(args)

stream = FALSE
Symbols = NULL
alerts = NULL

if(length(args) > 0 && args[1] == "compute")
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

  computeBacktest(Symbols, startDate, endDate, TRUE)
}

if(length(args) == 0 || args[1] == "stream")
{
  stream = TRUE

  if(length(args) > 1)
  {
    Symbols <- tail(args, n=(length(args)-1))
  }

  computeStream(Symbols)
}

warnings()

