source("startProbe.R")
source("filters.R")
source("poly-reg.R")

fsmState <- "startProbe"

while(fsmState != "end")
{
  if(fsmState == "startProbe")
  {
    SYMBOLS <- startProbe()
    fsmState <- "loadFilters"
  }
  else if(fsmState == "loadFilters")
  {
    loadFilters(SYMBOLS, c("polyreg"))
    fsmState <- "applyFilters"
  }
  else if(fsmState == "applyFilters")
  {
    #busca bloco de informacao (um dado de algum simbolo)
    #para o simbolo
    quoteName <- SYMBOLS[2]
    paranguaricutirimiruaru <- get(quoteName)
    fsmState <- "end"
  }
}