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

plotRegressions(Symbols)

