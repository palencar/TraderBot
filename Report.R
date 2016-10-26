library("data.table")

args <- commandArgs(trailingOnly=TRUE)

if(length(args) < 1)
  quit()
  
#lines = readLines(args[1])

#dataList <- c(list())
#length(dataList) <- length(lines)

#i <- 1

#for(line in lines)
#{
#  tokens <- strsplit(line, " ")[[1]]
#  
#  par   <- as.numeric(strsplit(tokens[1], "_")[[1]])
#  val   <- as.numeric(tokens[2])
#  proft <- as.numeric(tokens[3])
#  profp <- as.numeric(tokens[4])
#  
#  dataList[[i]] <- data.frame(t(par), val, proft, profp)
#  i <- i + 1
#}

#dataTable <- rbindlist(dataList)

dataTable <- data.frame(read.table(args[1], sep = " "))
  
png("reportX1.png")
plot(dataTable$V1, dataTable$V11, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
dev.off()

png("reportX2.png")
plot(dataTable$V2, dataTable$V11, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
dev.off()

png("reportX3.png")
plot(dataTable$V3, dataTable$V11, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
dev.off()

png("reportX4.png")
plot(dataTable$V4, dataTable$V11, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
dev.off()

png("reportX5.png")
plot(dataTable$V5, dataTable$V11, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
dev.off()

png("reportX6.png")
plot(dataTable$V6, dataTable$V11, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
dev.off()

png("reportX7.png")
plot(dataTable$V7, dataTable$V11, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
dev.off()

png("reportX8.png")
plot(dataTable$V8, dataTable$V11, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
dev.off()
