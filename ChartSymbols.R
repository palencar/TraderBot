source("startProbe.R")
source("filters.R")
source("polyReg.R")
source("chart.R")
source("orders.R")


args_cmd <- commandArgs(trailingOnly=TRUE)

symbolNames <- NULL

if(length(args_cmd) >= 1)
{
  symbolNames <- args_cmd
}

Symbols <- startProbe(symbolNames, FALSE)

#filterSymbols <- filterIncomplete(Symbols)

chartSymbols(Symbols, dev="png")
