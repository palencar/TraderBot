source("startProbe.R")
source("filters.R")
source("poly-reg.R")
source("plot.R")
source("orders.R")

require(compiler)
enableJIT(3)

Symbols <- startProbe()

args_cmd <- commandArgs(trailingOnly=TRUE)

if(length(args_cmd) >= 1)
{
  Symbols <- args_cmd
}

#filterSymbols <- filterIncomplete(Symbols)

for(symbol in Symbols)
{
  ptrnStr <- sprintf(".*%s.*r_.*rds", symbol)
  objFiles <- list.files("backtest", pattern=ptrnStr)
  
  print(symbol)
  
  Objects <- c()
  
  k <- 1
  
  for(name in objFiles)
  {
    fileName <- sprintf("backtest/%s", name)
    print(fileName)
    alertas <- readRDS(file=fileName)
    
    if(length(alertas) > 0)
    {
      for(i in 1:(length(alertas)-1))
      {
        Objects[[k]] <- alertas[[i]]
        k <- k+1
      }
    }
  }
  
  if(length(Objects) > 0)
  {
    plotPolyRegs(Objects, dev="png", showPositions=TRUE)
  }
}