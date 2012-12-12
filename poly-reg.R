"polyRegression" <- function (Symbol) 
{
  require(quantmod)

  #getSymbols("Symbol", src="yahoo")

  x <- as.integer(index(Symbol))
  y <- as.double((Hi(Symbol)+Lo(Symbol)+Cl(Symbol))/3)

  o = order(x)

  #do.it <- function (model, col) {
  #  r <- lm( model )
  #  yp <- predict(r)
  #  lines( yp[o] ~ x[o], col=col, lwd=3 )
  #}
  #
  #plot( y~x )
  #
  #x <- as.integer(index(Symbol))
  #
  #do.it(y~poly(x,2), col="blue")

  x <- as.Date(index(Symbol))

  #plot( y~x )

  x <- as.integer(index(Symbol))

  r <- lm(y~poly(x,2))

  yp <- predict(r)

  yr <- xts(yp, as.Date(x))

  chartSeries(Symbol,
	      TA=c(addTA(yr,on=1,col=3),
		  addTA(yr+(summary(r)$sigma),on=1,col=7),
		  regm <- addTA(yr-(summary(r)$sigma),on=1,col=7))
  )

  z <- x*0
  z[y > (yp+(summary(r)$sigma))] <- +1
  z[y < (yp-(summary(r)$sigma))] <- -1
  z[y > (yp+((summary(r)$sigma)*2))] <- +2
  z[y < (yp-((summary(r)$sigma)*2))] <- -2
}