
adjustOHLC.daily <- function(x, adjust=c("split","dividend"), use.Adjusted=FALSE,
                             ratio=NULL, symbol.name=deparse(substitute(x)))
{
  if(is.null(ratio)) {
    if(use.Adjusted) {
      # infer from Yahoo! Adjusted column
      if(!has.Ad(x))
        stop("no Adjusted column in 'x'")
      ratio <- Ad(x)/Cl(x)
    } else {
      # use actual split and/or dividend data
      div <- getDividends.db(symbol.name)[paste0("/", last(index(x)))]
      splits <- getSplits.db(symbol.name)[paste0("/", last(index(x)))]
      # un-adjust dividends for splits (Yahoo already adjusts div for splits)
      # do not use split.adjust=FALSE in getDividends call, which would
      # download the split data twice.
      if(is.xts(splits) && is.xts(div) && nrow(splits) > 0 && nrow(div) > 0)
        div <- div * 1/adjRatios(splits=base::merge(splits, index(div)))[,1]
      # calculate adjustment ratios using unadjusted dividends
      ratios <- adjRatios(splits, div, Cl(x))
      if(length(adjust)==1 && adjust == "split") {
        ratio <- ratios[,1]
      } else if(length(adjust)==1 && adjust == "dividend") {
        ratio <- ratios[,2]
      } else ratio <- ratios[,1] * ratios[,2]
    }
  }
  Adjusted <- Cl(x) * ratio
  structure(
    cbind((ratio * (Op(x)-Cl(x)) + Adjusted),
          (ratio * (Hi(x)-Cl(x)) + Adjusted),
          (ratio * (Lo(x)-Cl(x)) + Adjusted),
          Adjusted,
          if(has.Vo(x)) Vo(x) else NULL,
          if(has.Ad(x)) Ad(x) else NULL
    ),
    .Dimnames=list(NULL, colnames(x)))
}

adjustOHLC.intraday <- function (x, adjust = c("split", "dividend"), use.Adjusted = FALSE,
                                 ratio = NULL, symbol.name = deparse(substitute(x)))
{
  if (is.null(ratio)) {
    if (use.Adjusted) {
      if (!has.Ad(x))
        stop("no Adjusted column in 'x'")
      ratio <- Ad(x)/Cl(x)
    }
    else {
      div <- getDividends.db(symbol.name)[paste0("/", last(index(x)))]
      splits <- getSplits.db(symbol.name)[paste0("/", last(index(x)))]
      if (is.xts(splits) && is.xts(div) && nrow(splits) >
          0 && nrow(div) > 0)
        div <- div * 1/adjRatios.intraday(splits = base::merge(splits, index(div)))[, 1]
      ratios <- adjRatios.intraday(splits, div, Cl(x))
      if (length(adjust) == 1 && adjust == "split") {
        ratio <- ratios[, 1]
      }
      else if (length(adjust) == 1 && adjust == "dividend") {
        ratio <- ratios[, 2]
      }
      else ratio <- ratios[, 1] * ratios[, 2]
    }
  }
  Adjusted <- Cl(x) * ratio
  structure(
    cbind((ratio * (Op(x) - Cl(x)) + Adjusted),
          (ratio * (Hi(x) - Cl(x)) + Adjusted),
          (ratio * (Lo(x) - Cl(x)) + Adjusted),
          Adjusted,
          if (has.Vo(x)) Vo(x) else NULL,
          if (has.Ad(x)) Ad(x) else NULL
    ),
    .Dimnames = list(NULL, colnames(x)))
}

adjRatios.intraday <- function (splits, dividends, close)
{
  if (!missing(dividends) &&
      missing(close))
    stop("\"close\" must be specified to adjust dividends")

  if (missing(close) || all(is.na(close)) || NROW(close) == 0) {
    close <- NA
  }
  else {
    if (NCOL(close) != 1)
      stop("\"close\" must be univariate")
    close <- try.xts(close, error = stop("\"as.xts(close)\" failed"))
  }
  if (missing(splits) || all(is.na(splits)) || NROW(splits) == 0) {
    splits <- NA
  }
  else {
    if (NCOL(splits) != 1)
      stop("\"splits\" must be univariate")
    splits <- try.xts(splits, order.by=as.POSIXct(index(splits)), error = stop("\"as.xts(splits)\" failed"))
  }
  if (missing(dividends) || all(is.na(dividends)) || NROW(dividends) == 0) {
    dividends <- NA
  }
  else {
    if (NCOL(dividends) != 1)
      stop("\"dividends\" must be univariate")
    dividends <- try.xts(dividends, order.by=as.POSIXct(index(dividends)), error = stop("\"as.xts(dividends)\" failed"))
  }

  obj <- merge.xts(close, splits, dividends)
  if (!isTRUE(is.na(close))) {
    obj[,names(Cl(obj))] <- na.locf(Cl(obj))
    obj <- obj[!is.na(obj[, 1]), ]
  }
  adj <- .Call("adjRatios", obj[, 2], obj[, 3], obj[, 1], PACKAGE = "TTR")
  adj <- xts(cbind(adj[[1]], adj[[2]]), index(obj))

  colnames(adj) <- c("Split", "Div")

  return(adj)
}
