source("R/dbInterface.R")

Operations <- as.matrix(read.table(file="Acoes.csv", header=TRUE, sep=","))

for(i in 1:nrow(Operations))
{
  Operation <- as.matrix(Operations)[i,]
  symbol <- sprintf("%s.SA", Operation[1])
  date <- as.Date(sprintf("%s", Operation[2]), format = "%d/%m/%y")
  type <- Operation[3]
  size <- as.integer(Operation[4])
  price <- as.double(Operation[5])
  cost <- as.double(Operation[11])

  query <- sprintf("INSERT INTO operations (symbol, date, type, size, price, cost) VALUES ('%s', '%s', '%s', %s, %s, %s)", symbol, date, type, size, price, cost)

  print(query)
  fr <- getQuery(query)
}
