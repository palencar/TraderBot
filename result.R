
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
  cat(result, file=logFile, sep = "\n", append=TRUE)
  cmdLine <- sprintf("cat %s/%s.log | grep -v \"0.00\" | sort -u > %s/%s.bkp && mv result/%s.bkp %s/%s.log",
                     resultPath, symbol, resultPath, symbol, resultPath, symbol, resultPath, symbol)
  system(cmdLine)
}
