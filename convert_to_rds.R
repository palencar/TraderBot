args_cmd <- commandArgs(trailingOnly=TRUE)

for(name in args_cmd)
{
  rdsName <- gsub(".Robj", ".rds", x=name)
  strOut <- sprintf("Convertendo %s para %s", name, rdsName)
  print(strOut)
  
  object <- dget(name)
  saveRDS(object, file=rdsName)
  
  rmStr <- sprintf("rm %s", name)
  system(rmStr)
}
