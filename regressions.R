source("startProbe.R")
source("filters.R")
source("poly-reg.R")
source("plot.R")
source("utils.R")


args <- c()
args[1] <- as.Date(Sys.Date() - 30)
args[2] <- as.Date(Sys.Date())

args_cmd <- commandArgs(trailingOnly=TRUE)

if(length(args_cmd) == 2)
{
  args <- args_cmd
}

Symbols <- startProbe()

processRegressions(Symbols, args[1], args[2])

