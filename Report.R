library("data.table")

args <- commandArgs(trailingOnly=TRUE)

if(length(args) < 1)
  quit()

dataTable <- data.frame(read.table(args[1], sep = " "))
  
png("reportX1.png")
smaPeriod <- dataTable#[!duplicated(dataTable[,c('V1', 'V11')]),]
scatter.smooth(smaPeriod$V1, smaPeriod$V11, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
dev.off()

png("reportX2.png")
upperBand <- dataTable#[!duplicated(dataTable[,c('V2', 'V11')]),]
scatter.smooth(upperBand$V2, upperBand$V11, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
dev.off()

png("reportX3.png")
lowerBand <- dataTable#[!duplicated(dataTable[,c('V3', 'V11')]),]
scatter.smooth(lowerBand$V3, lowerBand$V11, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
dev.off()

png("reportX4.png")
downChange <- dataTable#[!duplicated(dataTable[,c('V4', 'V11')]),]
scatter.smooth(downChange$V4, downChange$V11, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
dev.off()

png("reportX5.png")
upChange <- dataTable#[!duplicated(dataTable[,c('V5', 'V11')]),]
scatter.smooth(upChange$V5, upChange$V11, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
dev.off()

png("reportX6.png")
lowerLimit <- dataTable#[!duplicated(dataTable[,c('V6', 'V11')]),]
scatter.smooth(lowerLimit$V6, lowerLimit$V11, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
dev.off()

png("reportX7.png")
stopGain <- dataTable#[!duplicated(dataTable[,c('V7', 'V11')]),]
scatter.smooth(stopGain$V7, stopGain$V11, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
dev.off()

png("reportX8.png")
stopLoss <- dataTable#[!duplicated(dataTable[,c('V8', 'V11')]),]
scatter.smooth(stopLoss$V8, stopLoss$V11, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
dev.off()
