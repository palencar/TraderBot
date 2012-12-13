loadFilters <- function(Symbols = NULL, Filters = NULL)
{
  str(Symbols)
  for (i in 1:length(Symbols))
  {
    for (j in 1:length(Filters))
    {
      str(Symbols[i])
      str(Filters[j])
      get(Symbols[i])
      #Symbols[[i]] <- merge(Symbols[[i]], as.numeric(0))# colocar o nome
    }
  }
}

## Funcoes de filtro