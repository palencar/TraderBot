library("data.table")

args <- commandArgs(trailingOnly=TRUE)

if(length(args) < 1)
  quit()

files <- list.files(args[1], pattern = "*.txt")

data <- list()
i <- 1

for(file in files)
{
  print(file)
  name <- paste("result", file, sep = "/")
  obj <- data.frame(read.table(name, sep = " "))
  obj$symbol <- unlist(strsplit(file, "[.]"))[1]
  data[[i]] <- obj
  i <- i + 1
}

dataTable <- rbindlist(data)
  
smaPeriod <- data.frame(dataTable$V1, dataTable$V11)
smaPeriod <- smaPeriod[complete.cases(smaPeriod),]
if(nrow(smaPeriod) > 0)
{
  png("smaPeriod.png")
  colnames(smaPeriod) <- c("smaPeriod", "proffit")
  scatter.smooth(smaPeriod, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
  dev.off()
}

upperBand <- data.frame(dataTable$V2, dataTable$V11)
upperBand <- upperBand[complete.cases(upperBand),]
if(nrow(upperBand) > 0)
{
  png("upperBand.png")
  colnames(upperBand) <- c("upperBand", "proffit")
  scatter.smooth(upperBand, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
  dev.off()
}

lowerBand <- data.frame(dataTable$V3, dataTable$V11)
lowerBand <- lowerBand[complete.cases(lowerBand),]
if(nrow(lowerBand) > 0)
{
  png("lowerBand.png")
  colnames(lowerBand) <- c("lowerBand", "proffit")
  scatter.smooth(lowerBand, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
  dev.off()
}

downChange <- data.frame(dataTable$V4, dataTable$V11)
downChange <- downChange[complete.cases(downChange),]
if(nrow(downChange) > 0)
{
  png("downChange.png")
  colnames(downChange) <- c("downChange", "proffit")
  scatter.smooth(downChange, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
  dev.off()
}

upChange <- data.frame(dataTable$V5, dataTable$V11)
upChange <- upChange[complete.cases(upChange),]
if(nrow(upChange) > 0)
{
  png("upChange.png")
  colnames(upChange) <- c("upChange", "proffit")
  scatter.smooth(upChange, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
  dev.off()
}

lowerLimit <- data.frame(dataTable$V6, dataTable$V11)
lowerLimit <- lowerLimit[complete.cases(lowerLimit),]
if(nrow(lowerLimit) > 0)
{
  png("lowerLimit.png")
  colnames(lowerLimit) <- c("lowerLimit", "proffit")
  scatter.smooth(lowerLimit, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
  dev.off()
}

stopGain <- data.frame(dataTable$V7, dataTable$V11)
stopGain <- stopGain[complete.cases(stopGain),]
if(nrow(stopGain) > 0)
{
  png("stopGain.png")
  colnames(stopGain) <- c("stopGain", "proffit")
  scatter.smooth(stopGain, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
  dev.off()
}

stopLoss <- data.frame(dataTable$V2, dataTable$V11)
stopLoss <- stopLoss[complete.cases(stopLoss),]
if(nrow(stopLoss) > 0)
{
  png("stopLoss.png")
  colnames(stopLoss) <- c("stopLoss", "proffit")
  scatter.smooth(stopLoss, col=rgb(0,100,0,50,maxColorValue=255), pch=16)
  dev.off()
}

symbols <- dataTable$symbol
symbols <- symbols[!duplicated(symbols)]
profList <- list()

i <- 1
for(symbol in symbols)
{
  index <- which(dataTable$symbol == symbol)
  proffit <- mean(dataTable[index]$V11)
  profList[[i]] <- data.frame(symbol, proffit)
  i <- i + 1
}

meanProffit <- rbindlist(profList)
proffitOrder <- meanProffit[with(meanProffit, order(proffit)), ]
print(proffitOrder)

#dff <- NULL
#data <- list()
#i <- 1
#
#for(file in files)
#{
#  symbol <- unlist(strsplit(file, "[.]"))[1]
#  name <- paste("result", file, sep = "/")
#  obj <- data.frame(read.table(name, sep = " "))
#  proffit <- mean(obj$V11)
#  #volatility <- mean(na.omit(volatility(get(symbol))))
#  #df <- data.frame(proffit, symbol, volatility)
#  df <- data.frame(proffit, symbol)
#  dff <- rbind(dff, df)
#}
#
#negative <- df[df$proffit < 0, ]$symbol
#positive <- df[df$proffit > 0, ]$symbol
#AllSymbols <- startProbe(symbolNames = df$proffit, minAge=200, update=FALSE)
#
#for(symbol in positive)
#{
#  sprintf("%s %.2f", symbol, volatility(get(symbol)))
#}
