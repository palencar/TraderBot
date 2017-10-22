smaSD <- function(symbol, n=500, nma=500)
{
  smasd <- smaSDdata(symbol, n, nma)

  if(is.null(smasd))
    NULL

  assign("smasd", smasd[, c('up', 'mavg', 'dn')], .GlobalEnv)
  "addTA(smasd, on=1, col=c(8,2,8))"
}

smaSDdata <- function(symbol, n=500, nma = 500)
{
  if(nrow(symbol) <= (n + nma))
  {
    warning(sprintf("smaSD: length: %d <= (%d + %d)", length(seq), n, nma))
    return(NULL)
  }

  seq <- xts((Hi(symbol)+Lo(symbol)+Cl(symbol))/3)
  sma <- SMA(seq, nma)
  dif <- seq-sma
  ssd <- runSD(dif, n, sample = TRUE)
  smasd <- na.omit(cbind(sma+(2*ssd), sma, sma-(2*ssd), dif, ssd))
  names(smasd) <- c('up', 'mavg', 'dn', 'dif', 'sd')
  smasd
}
