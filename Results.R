
filePath <- "training"

objFiles <- list.files(filePath, pattern="*log")

for(logFile in objFiles)
{
  lines = readLines(sprintf("%s/%s", filePath, logFile))
  
  positions <- NULL
  
  for(line in lines)
  {
    elements <- unlist(strsplit(line, " "))
    
    if(elements[3] == "sell")
    {
      if(is.null(positions) == FALSE)
      {
        sell_price <- (length(positions)*as.double(elements[4])*100)
        buy_price <- (sum(positions)*100)
        print(paste(elements[1], (100*length(positions)), (sell_price - buy_price), ((sell_price - buy_price) / buy_price)))
        positions <- NULL
      }
    }
        
    if(elements[3] == "buy")
    {
      positions <- c(positions, as.double(elements[4]))
    }
  }
  
  if(is.null(positions) == FALSE)
  {
    #sell_price <- (length(positions)*as.double(elements[4])*100)
    buy_price <- (sum(positions)*100)
    print(paste(elements[1], buy_price)) # TODO buscar o preco do ultimo negocio, explicitar que a posicao esta em aberto
    positions <- NULL
  }
}

