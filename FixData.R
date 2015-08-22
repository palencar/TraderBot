source("startProbe.R")

conn <- file("baddata.txt", "r")
while(length(line <- readLines(conn, 1)) > 0) {
  str <- strsplit(line, " ")[[1]]
  print(str[1])
  getQuoteDay(str[1], str[2])
}

file.remove("baddata.txt")