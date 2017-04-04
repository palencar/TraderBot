source("R/dbInterface.R")


getOrders <- function(name, pos = NULL)
{
  if(is.null(pos))
  {
    pos <- getPositions(name)
  }

  symbol <- base::get(name)

  if(length(pos) == 0)
  {
    return()
  }

  posit <- NULL
  i <- 0

  for(reg in pos)
  {
    if(is.na(reg$end) == TRUE)
    {
      endDate <- time(tail(symbol, n=1))
    }
    else
    {
      endDate <- reg$end
    }

    startDate <- as.Date(reg$start)

    period <- sprintf("%s::%s", startDate, endDate)

    firstVal <- reg$openVal

    firstReg <- head(symbol[period], n=1)
    lastReg <- tail(symbol[period], n=1)

    xNew = xts(rep(NA,length(index(symbol[period]))), index(symbol[period]))

    if(firstVal == 0)
      xNew[time(firstReg)] <- as.double(Cl(firstReg))
    else
      xNew[time(firstReg)] <- firstVal

    if(is.na(reg$closeVal) == FALSE)
    {
      xNew[time(lastReg)] <- reg$closeVal
    }
    else
    {
      xNew[time(lastReg)] <- as.double(Cl(lastReg))
    }

    if(as.double(xNew[time(firstReg)]) < as.double(xNew[time(lastReg)]))
      col <- 3
    else
      col <- 2

    i <- i + 1
    objName <- sprintf("%s.p%d", name, i)
    position <- xts(na.approx(xNew))
    assign(objName, position, .GlobalEnv)

    posit <- c(posit, sprintf("addTA(%s, lwd=2, on=1, col=%d)", objName, col))
  }

  return(posit)
}
