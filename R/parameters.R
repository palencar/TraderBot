source("R/report.R")


mMergeBacktest <- memoise(mergeBacktest)

getParameters <- function(timeFrame, operation="trade")
{
  if(operation == "trade")
    return(tdParameters(timeFrame))

  if(operation == "backtest")
    return(btParameters(timeFrame))

  return(NULL)
}

tdParameters <- function(timeFrame)
{
  pars <- read.csv("tradeParameters.csv", row.names = 1)

  pars[timeFrame, ]
}

btParameters <- function(timeFrame)
{
  df <- mMergeBacktest()
  df <- df[df$timeframe == timeFrame & df$state == 'closed' & df$stopLoss < 1, ]
  df <- head(df[order(df$proffit_pp, decreasing = TRUE), ], as.integer(0.2 * nrow(df)))
  df <- df[sample(nrow(df), as.integer(0.2 * nrow(df)), TRUE),]

  if(nrow(df) == 0)
  {
    names <- colnames(df)
    df <- data.frame(matrix(rep(0, ncol(df)*2), nrow=2))
    colnames(df) <- names
    parNames <- c("smaPeriod", "lowerBand", "upperBand", "upChange", "downChange", "lowLimit", "stopGain", "stopLoss", "bullBuy", "bullSell", "bearSell", "bearBuy")
    df[1, parNames] <- c(300, -1, 1, 4, -4, 0.4, 3, 0.4, 0.4, 0.4, 0.4, 0.4)
    df[2, parNames] <- c(400, -2, 2, 5, -5, 0.6, 4, 0.6, 0.6, 0.6, 0.6, 0.6)
  }

  randPar <- function(min, max, meanValue, stdDev)
  {
    val <- rnorm(1, meanValue, stdDev)

    while(val < min || val > max)
      val <- rnorm(1, meanValue, stdDev)

    return(as.numeric(formatC(val, digits=2,format="f")))
  }

  smaPeriod = round(randPar(config$backtest$sma_period$min, config$backtest$sma_period$max, mean(df$smaPeriod), sd(df$smaPeriod)))

  upperBand = randPar(config$backtest$upper_band$min, config$backtest$upper_band$max, mean(df$upperBand), sd(df$upperBand))

  lowerBand = randPar(config$backtest$lower_band$min, config$backtest$lower_band$max, mean(df$lowerBand), sd(df$lowerBand))

  upChange = randPar(config$backtest$up_change$min, config$backtest$up_change$max, mean(df$upChange), sd(df$upChange))

  downChange = randPar(config$backtest$down_change$min, config$backtest$down_change$max, mean(df$downChange), sd(df$downChange))

  lowLimit = randPar(config$backtest$low_limit$min, config$backtest$low_limit$max, mean(df$lowLimit), sd(df$lowLimit))

  stopLoss = randPar(config$backtest$stop_loss$min, config$backtest$stop_loss$max, mean(df$stopLoss), sd(df$stopLoss))

  stopGain = randPar(config$backtest$stop_gain$min, config$backtest$stop_gain$max, mean(df$stopGain), sd(df$stopGain))

  bearSell  = randPar(config$backtest$bear_sell$min, config$backtest$bear_sell$max, mean(df$bearSell), sd(df$bearSell))

  bearBuy  = randPar(config$backtest$bear_buy$min, config$backtest$bear_buy$max, mean(df$bearBuy), sd(df$bearBuy))

  bullBuy  = randPar(config$backtest$bull_buy$min, config$backtest$bull_buy$max, mean(df$bullBuy), sd(df$bullBuy))

  bullSell  = randPar(config$backtest$bull_sell$min, config$backtest$bull_sell$max, mean(df$bullSell), sd(df$bullSell))

  data.frame(smaPeriod, upperBand, lowerBand, upChange, downChange, lowLimit, stopLoss, stopGain, bearSell, bearBuy, bullBuy, bullSell)
}

