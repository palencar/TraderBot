source("R/dbInterface.R")


getOrders <- function(name, endDate = NULL, mode = "operation", adjusted = TRUE)
{
  timeFrame <- NULL
  if(mode == "simulation")
  {
    timeFrame <- unlist(strsplit(name, "[.]"))[2]
    if(is.na(timeFrame))
      timeFrame <- "1D"
  }

  pos <- getPositions(unlist(strsplit(name, "[.]"))[1], timeFrame, endDate, mode = mode)

  symbol <- base::get(name)

  if(length(pos) == 0)
    return(NULL)

  posit <- NULL
  i <- 0

  for(reg in pos)
  {
    if(!is.na(reg$end))
      ed <- reg$end
    else if(!is.null(endDate))
      ed <- endDate
    else
      ed <- time(tail(symbol, n=1))

    period <- sprintf("%s::%s", reg$start, ed)

    if(nrow(symbol[period]) == 0)
      next

    firstVal <- reg$openVal

    firstReg <- head(symbol[period], n=1)
    lastReg <- tail(symbol[period], n=1)

    xNew = xts(rep(NA, length(index(symbol[period]))), index(symbol[period]))
    xNew[time(firstReg)] <- firstVal

    if(is.na(reg$closeVal) == FALSE)
      xNew[time(lastReg)] <- reg$closeVal
    else
      xNew[time(lastReg)] <- as.double(Cl(lastReg))

    if(adjusted)
      xNew <- adjustOperations(unlist(strsplit(name, "[.]"))[1], xNew)

    if(as.double(xNew[time(firstReg)]) < as.double(xNew[time(lastReg)]))
      col <- ifelse(mode == "operation", 3, 4)
    else
      col <- ifelse(mode == "operation", 2, 7)

    i <- i + 1
    objName <- sprintf("%s.p%d", name, i)
    position <- xts(na.approx(as.double(xNew)), order.by = index(xNew))

    assign(objName, position, .GlobalEnv)

    posit <- c(posit, sprintf("addTA(%s, lwd=2, on=1, col=%d)", objName, col))
  }

  return(posit)
}
