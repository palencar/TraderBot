source("R/report.R")


mMergeBacktest <- memoise(mergeBacktest)

predictBest <- function(df = mMergeBacktest(), colName)
{
  df <- df[df$maxDrawdown != 0] #prune elements
  df <- df[, c('grade', colName), with=FALSE]
  df <- df[,list(grade=mean(grade)), by=colName]

  p <- loess(formula(paste0("grade ~ ", colName)), df)
  df[which.max(predict(p)), colName, with=FALSE]
}

getParameters <- function(timeFrame, operation="trade", fileName = config::get()$parameters)
{
  if(operation == "trade")
    return(tdParameters(timeFrame, fileName))

  if(operation == "backtest")
    return(btParameters(timeFrame))

  return(NULL)
}

tdParameters <- function(timeFrame, fileName)
{
  pars <- read.csv(fileName, row.names = 1)

  pars[timeFrame, ]
}

btParameters <- function(timeFrame = NULL, state = NULL, type = NULL)
{
  bt <- config$backtest

  df <- mMergeBacktest()
  df <- df[ df$smaPeriod  >= bt$sma_period$min  & df$smaPeriod  <= bt$sma_period$max &
            df$upperBand  >= bt$upper_band$min  & df$upperBand  <= bt$upper_band$max &
            df$lowerBand  >= bt$lower_band$min  & df$lowerBand  <= bt$lower_band$max &
            df$stopGainLong  >= bt$stop_gain_long$min  & df$stopGainLong <= bt$stop_gain_long$max &
            df$stopGainShort >= bt$stop_gain_short$min & df$stopGainShort <= bt$stop_gain_short$max &
            df$stopLoss   >= bt$stop_loss$min   & df$stopLoss   <= bt$stop_loss$max ]
  if(!is.null(timeFrame))
    df <- df[df$timeframe == timeFrame, ]
  if(!is.null(state))
    df <- df[df$state == state,]
  if(!is.null(type))
    df <- df[df$type == type,]
  df <- df[sample(nrow(df), as.integer(0.3 * nrow(df)), TRUE),]

  parNames <- c("smaPeriod", "lowerBand", "upperBand", "stopGainLong", "stopGainShort", "stopLoss")

  if(nrow(df) <= 200)
  {
    df <- data.frame(matrix(rep(0, 8*2), nrow=2))
    colnames(df) <- parNames
    df[1, parNames] <- c(bt$sma_period$min, bt$lower_band$min, bt$upper_band$min, bt$high_limit$min, bt$low_limit$min, bt$stop_gain_long$min, bt$stop_gain_short$min, bt$stop_loss$min)
    df[2, parNames] <- c(bt$sma_period$max, bt$lower_band$max, bt$lower_band$max, bt$high_limit$max, bt$low_limit$max, bt$stop_gain_long$max, bt$stop_gain_short$max, bt$stop_loss$max)
    dF <- as.data.table(df)
  }
  else
  {
    dF <- data.frame(lapply(parNames, function(x) {predictBest(df=df, colName=x)}))
    colnames(dF) <- parNames
  }

  randPar <- function(min, max, meanValue, stdDev)
  {
    val <- rnorm(1, meanValue, stdDev)

    if(is.null(meanValue) || is.na(meanValue) || meanValue > max || meanValue < min)
      meanValue <- (min+max)/2

    while(val < min || val > max)
      val <- rnorm(1, meanValue, stdDev)

    return(as.numeric(formatC(val, digits=2,format="f")))
  }

  smaPeriod = round(randPar(bt$sma_period$min, bt$sma_period$max, dF$smaPeriod, sd(df$smaPeriod)))
  upperBand = randPar(bt$upper_band$min, bt$upper_band$max, dF$upperBand, sd(df$upperBand))
  lowerBand = randPar(bt$lower_band$min, bt$lower_band$max, dF$lowerBand, sd(df$lowerBand))
  stopLoss = randPar(bt$stop_loss$min, bt$stop_loss$max, dF$stopLoss, sd(df$stopLoss))
  stopGainLong  = randPar(bt$stop_gain_long$min,  bt$stop_gain_long$max,  dF$stopGainLong,  sd(df$stopGainLong))
  stopGainShort = randPar(bt$stop_gain_short$min, bt$stop_gain_short$max, dF$stopGainShort, sd(df$stopGainShort))

  data.frame(smaPeriod, upperBand, lowerBand, stopLoss, stopGainLong, stopGainShort)
}

