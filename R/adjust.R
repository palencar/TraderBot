
adjustOperations <- function(symbolName, op)
{
  symbolName <- unlist(strsplit(symbolName, "[.]"))[1]

  df <- xts(data.frame(Open=op, High=op, Low=op, Close=op), order.by = index(op))
  names(df) <- c("Open", "High", "Low", "Close")

  df <- adjustOHLC.db(df, symbol.name = symbolName, adjust = c('split', 'dividend'))

  df <- Cl(df)
  names(df) <- "price"

  df
}

adjustOHLC.db <- function (x, adjust = c("split", "dividend"), use.Adjusted = FALSE,
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
        div <- div * 1/adjRatios.db(splits = base::merge(splits, index(div)))[, 1]
      ratios <- adjRatios.db(splits, div, Cl(x))
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

adjRatios.db <- function (splits, dividends, close)
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
