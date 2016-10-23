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
plot(dataTable$V1, dataTable$V8)
dev.off()

png("reportX2.png")
plot(dataTable$V2, dataTable$V8)
dev.off()

png("reportX3.png")
plot(dataTable$V3, dataTable$V8)
dev.off()

png("reportX4.png")
plot(dataTable$V4, dataTable$V8)
dev.off()

png("reportX5.png")
plot(dataTable$V5, dataTable$V8)
dev.off()

