require(quantmod)

getSymbols("CPFE3.SA", src="yahoo")

x <- as.integer(index(CPFE3.SA))
y <- as.double((Hi(CPFE3.SA)+Lo(CPFE3.SA)+Cl(CPFE3.SA))/3)

o = order(x)

#do.it <- function (model, col) {
#  r <- lm( model )
#  yp <- predict(r)
#  lines( yp[o] ~ x[o], col=col, lwd=3 )
#}
#
#plot( y~x )
#
#x <- as.integer(index(CPFE3.SA))
#
#do.it(y~poly(x,2), col="blue")

x <- as.Date(index(CPFE3.SA))

#plot( y~x )

x <- as.integer(index(CPFE3.SA))

r <- lm(y~poly(x,2))

yp <- predict(r)

yr <- xts(yp, as.Date(x))

chartSeries(CPFE3.SA,
  TA=c(addTA(yr,on=1,col=3),
  addTA(yr+(summary(r)$sigma),on=1,col=7),
  regm <- addTA(yr-(summary(r)$sigma),on=1,col=7))
)
