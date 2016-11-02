library(data.table)
source("dbInterface.R")
source("result.R")

args <- commandArgs(trailingOnly=TRUE)

openTotalBuy <- 0
openTotalSell <- 0

closeTotalBuy <- 0
closeTotalSell <- 0

filePath <- "result"

showAll <- FALSE
report <- FALSE

if(length(args) >= 2 && "--all" %in% args)
  showAll <- TRUE

if(length(args) >= 2 && "--report" %in% args)
  report <- TRUE

if(length(args) >= 1)
  filePath <- args[length(args)]

for(name in files)
{
  obj <- readRDS(sprintf("%s/%s", filePath, name))

  pars <- unique(ls(obj))
  
  dataList <- c(list())
  length(dataList) <- length(pars)
  
  i <- 1
  for(key in pars)
  {
    singleResult(key, obj[[key]], report = report, showAll = showAll)
  }
}
