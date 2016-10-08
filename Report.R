args <- commandArgs(trailingOnly=TRUE)

if(length(args) < 1)
  quit()
  
lines = readLines(args[1])

dataList <- c(list())
length(dataList) <- length(lines)

i <- 1

for(line in lines)
{
  tokens <- strsplit(line, " ")[[1]]
  
  par   <- as.numeric(strsplit(tokens[1], "_")[[1]])
  val   <- as.numeric(tokens[2])
  proft <- as.numeric(tokens[3])
  profp <- as.numeric(tokens[4])
  
  dataList[[i]] <- data.frame(t(par), val, proft, profp)
  i <- i + 1
}

dataTable <- rbindlist(dataList)
  
png("reportX1.png")
plot(dataTable$X1, dataTable$profp)
dev.off()

png("reportX2.png")
plot(dataTable$X2, dataTable$profp)
dev.off()

png("reportX3.png")
plot(dataTable$X3, dataTable$profp)
dev.off()

png("reportX4.png")
plot(dataTable$X4, dataTable$profp)
dev.off()

png("reportX5.png")
plot(dataTable$X5, dataTable$profp)
dev.off()

