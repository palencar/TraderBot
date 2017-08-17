library("RcppEigen")
library("foreach")


polyRegression <- function (SymbolName, DateInterval, Period)
{
  Symbol <- base::get(SymbolName)[DateInterval]

  y <- as.numeric((Hi(Symbol)+Lo(Symbol)+Cl(Symbol))/3)

  x <- 1:nrow(Symbol)

  r <- RcppEigen::fastLm(poly(x,2), y)

  yp <- predict(r)+mean(r$residuals)

  yr <- xts(yp, index(Symbol))

  return(list(regression=yr, sigma=sd(r$residuals), name=SymbolName,
              interval=DateInterval, trend=revertTrend(yr, n=3), period=Period))
}


findCurves <- function(SymbolName, minDays, maxDays, dateLimit="")
{
  lista <- foreach (i = minDays:maxDays, .combine = rbind, .errorhandling="remove") %do%
  {
    list(polyRegression(SymbolName, sprintf("%s::%s", index(first(tail(base::get(SymbolName)[paste0("/", dateLimit)], i))), dateLimit), i))
  }

  return(lista)
}

changeRatio <- function(regIndicator)
{
  interval <- unlist(strsplit(regIndicator$interval, "::"))
  first <- interval[1]
  last  <- interval[2]

  days <- as.numeric(difftime(last, first), units = "days")

  return(abs(as.double(first(regIndicator$regression)) - as.double(last(regIndicator$regression))) / (days/30))
}

getPolyRegs <- function(Symbol, endDate=NULL)
{
  envObj <- new.env(hash=T, parent=emptyenv())
  alerts <- NULL

  objFile <- paste0("datacache/", Symbol, "_turnpoints.rds")
  if(file.exists(objFile))
  {
    envObj <- readRDS(file=objFile)
  }

  if(is.null(endDate))
    endDate <- Sys.time()

  k <- 1
  polyRegs <- c()

  for(key in ls(envObj))
  {
    alert <- envObj[[key]]

    if(length(alert) > 0)
    {
      for(i in 1:(length(alert)-1))
      {
        if(as.character.Date(key) <= as.character.Date(endDate) && changeRatio(alert[[i]]) > 1.5) #1.5% a.m.
        {
          objName <- sprintf("poly%s.p%d", Symbol, k)
          assign(objName, alert[[i]]$regression, .GlobalEnv)
          polyRegs <- c(polyRegs, sprintf("addTA(%s, on=1, col=3)", objName))

          objName <- sprintf("poly%s.p%dpsigma", Symbol, k)
          assign(objName, alert[[i]]$regression+alert[[i]]$sigma, .GlobalEnv)
          polyRegs <- c(polyRegs, sprintf("addTA(%s, lwd=2, on=1, col=7)", objName, col))

          objName <- sprintf("poly%s.p%dmsigma", Symbol, k)
          assign(objName, alert[[i]]$regression-alert[[i]]$sigma, .GlobalEnv)
          polyRegs <- c(polyRegs, sprintf("addTA(%s, lwd=2, on=1, col=7)", objName, col))

          k <- k+1
        }
      }
    }
  }

  return(polyRegs)
}
