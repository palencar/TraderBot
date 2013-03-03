source("startProbe.R")
source("filters.R")
source("poly-reg.R")
source("plot.R")

#if(exists(Symbols) == FALSE)
#{
Symbols <- startProbe()
#}

args_cmd <- commandArgs(trailingOnly=TRUE)

for(name in args_cmd)
{
  #print(name)
  plotObjectSet(name, dev="png")
}
