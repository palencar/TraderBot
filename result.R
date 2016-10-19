
writeResult <- function(symbol, result, parameters = NULL)
{
  resultPath <- "result"
  dir.create(resultPath, showWarnings=FALSE)

  if(!is.null(parameters))
  {
    resultPath <- sprintf("%s/%s", resultPath, paste(parameters, collapse = ""))
  }
  else
  {
    resultPath <- sprintf("%s/default", resultPath)
  }
  
  dir.create(resultPath, showWarnings=FALSE)

  logFile <- paste(resultPath,"/",symbol,".log", sep = "")
  
  lines <- NULL
  if(file.exists(logFile))
  {
    lines <- readLines(logFile)
  }
  
  if((result %in% lines) == FALSE)
  {
    if(is.null(lines) || lines[length(lines)] < result)
    {
      cat(result, file=logFile, sep = "\n", append=TRUE)
    }
    else if(!(result %in% lines))
    {
      lines <- sort(unique(c(lines, result)))
      fileConn <- file(logFile)
      writeLines(lines, fileConn)
      close(fileConn)
    }
  }
}

simPrice <- function(symbol, tradeDate, parameters = NULL)
{
  resultPath <- "result"
  
  if(!is.null(parameters))
  {
    resultPath <- sprintf("%s/%s", resultPath, paste(parameters, collapse = ""))
  }
  else
  {
    resultPath <- sprintf("%s/default", resultPath)
  }
  
  logFile <- paste(resultPath,"/",symbol,".log", sep = "")
  
  if(!file.exists(logFile))
  {
    return(NULL)
  }
  
  sim <- read.table(logFile)
  
  buy <- sim$V4[intersect(which(as.Date(sim$V2) <= as.Date(tradeDate)), which(sim$V3 == "buy"))]
  
  num <- length(buy)
  
  if(num == 0)
    return(NULL)
  
  return(sum(buy)/num)
}

