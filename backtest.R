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
  objects <- dget(name)
  
  for(i in 1:length(objects$names))
  {
    object <- objects[[i]]
    
    lastDay <- as.Date(xts::last(index(object$regression)))
    lastReg <- get(object$name)[lastDay]
    lastDayVal <- as.double((Hi(lastReg)+Lo(lastReg)+Cl(lastReg))/3)

    dateInterval <- sprintf("%s::", seq(as.Date(lastDay), length=2, by="1 days")[2])
    
    if(length(get(object$name)[dateInterval]) == 0)
      next
    
    maxHiVal <- Hi(get(object$name)[dateInterval])[which.max(Hi(get(object$name)[dateInterval]))]
    
    diffDays <- as.integer(difftime(as.POSIXlt.Date(index(maxHiVal)), as.POSIXlt.Date(lastDay)))
    diffVal  <- as.double(maxHiVal) - as.double(lastDayVal)
    changeRate <- as.double(diffVal/lastDayVal)*100
    
    strOut <- sprintf("%s %s %d %f %f", object$name, object$interval, diffDays, diffVal, changeRate)
    print(strOut)
  }
}
