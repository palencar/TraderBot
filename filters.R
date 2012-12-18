source('poly-reg.R')

loadFilters <- function(Symbols = NULL, Filters = NULL)
{
  str(Symbols)
  for (i in 1:length(Symbols))
  {
    for (j in 1:length(Filters))
    {
      #str(Symbols[i])
      #str(Filters[j])
      #get(Symbols[i])
      #Symbols[[i]] <- merge(Symbols[[i]], as.numeric(0))# colocar o nome
    }
  }
}

filterPolyReg <- function(SymbolNames, minDays, maxDays, Sigma)
{
  j <- 1
  lista <- list()
  for(i in 1:length(SymbolNames))
  {
    reg <- findBestCurve(SymbolNames[i], minDays, maxDays)
    
    lastDayDate <- time(last(reg$regression))
    
    if(Lo(get(SymbolNames[i])[lastDayDate] ) < last(reg$regression[lastDayDate])-(Sigma*reg$sigma))
    {
      lista[[j]] <- reg
      j <- j + 1
    }
  }
  return(lista)
}
  
filterTrendLine <- function()
{
  
}
