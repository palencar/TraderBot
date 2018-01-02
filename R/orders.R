source("R/dbInterface.R")


getOrders <- function(name, endDate = Sys.time(), mode = "operation", adjusted = TRUE)
{
  timeFrame <- NULL
  if(mode == "simulation")
  {
    timeFrame <- unlist(strsplit(name, "[.]"))[2]
    if(is.na(timeFrame))
      timeFrame <- "1D"
  }

  pos <- getPositions(unlist(strsplit(name, "[.]"))[1], timeFrame, as.Date(endDate), mode = mode)

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

    firstReg <- head(symbol[period], n=1)
    lastReg <- tail(symbol[period], n=1)

    idx  <- unique(c(index(firstReg), index(lastReg)))
    xNew <- xts(rep(NA, length(idx)), order.by = idx)
    xNew[time(lastReg)] <- ifelse(is.na(reg$closeVal) == FALSE, reg$closeVal, as.double(Cl(lastReg)))
    xNew[time(firstReg)] <- reg$openVal
    xNew <- rbind(xNew, Cl(symbol[index(symbol) > index(lastReg) & as.Date(index(symbol)) <= as.Date(endDate)]))

    if(adjusted && nrow(xNew) > 1)
      xNew <- adjustOperations(unlist(strsplit(name, "[.]"))[1], xNew)

    xt <- xts(rep(NA, length(index(symbol[period]))), order.by = index(symbol[period]))
    xt[idx] <- xNew[idx]
    xNew <- xt

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
