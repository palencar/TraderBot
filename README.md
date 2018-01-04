# TraderBot

# Installation

## Ubuntu Dependencies
```
sudo apt-get install -y r-base r-base-dev sqlite3 libxml2-dev libmysqlclient-dev libssl-dev libssh2-1-dev libcurl4-openssl-dev
```

## Clone repository
Clone the git repository.
```
git clone https://github.com/palencar/TraderBot
cd TraderBot
```

## Using MySQL Database
Create the database using the schema file *mysql-schema.sql*.
Uncomment the following lines o the file *config.yml*. Set the values for host, database, user and password.
```yml
engine:   "mysql"
host:     "db_hostname"
database: "db_name"
user:     "db_username"
password: "db_password"
```

## Using SQLite Database
Create the database from the schema file.
```
sqlite3 db.sqlite < sqlite3-schema.sql
```

Uncoment the following lines on the file *config.yml*:
```yml
engine:   "sqlite"
database: "db.sqlite"
```

## Install Packages on R
Start a R session.
```
R
```

Install devtools Package.

```R
install.packages("devtools")
```

Install TraderBot and dependencies.
```R
library(devtools)
install_git(".")
```

# Using

## Load TraderBot
```R
library(TraderBot)
```

# Download Data

## intraday
Download daily data from google.
```R
saveSymbolsDaily("BVMF3")
```
## Daily
Download last *15* days of intraday data from google.
```R
saveSymbolsIntraday("BVMF3")
```

## Splits and Dividends
```R
updateAdjust("BVMF3")
```

## Notes
The data is automatically updated only on live trading.
If there's not enough data, the operation (trade, simulation or backtest) will be skipped.

# Backtest
Test strategy for *minSamples* random parameters. The parameter limits are defined on *config.yml*.
```R
computeBacktest(Symbols = "BVMF3", minSamples = 1000, timeFrame="1D")
```

# Simulate
Test strategy using the values on the *tradeParameters.csv* file. Every line defines the
parameters for a given time frame.
```R
computeSimulation(Symbols = "BVMF3", timeFrame="1D")
```

# Live Trading
Update assets on database and execute the strategy. The parameter *timeFrame* can be one of the following:
1D, 1H, 30M, 15M, 10M and 5M.
```R
computeStream(Symbols = "BVMF3", timeFrames = c("5M", "10M", "15M", "30M", "1H", "1D"))
```
If an alert is triggered, the results ar shown on the terminal like below. The chart is written on *charts* directory.
```
[1] "Chart [RPMG3] [15M] [2018-01-03 16:40:00]: buy"
[1] "Chart [EMBR3] [30M] [2018-01-03 13:00:00]: sell"
[1] "Chart [CTKA4] [15M] [2018-01-02 14:18:00]: buy"
[1] "Chart [SPXI11] [30M] [2018-01-02 11:16:00]: sell"
  symbol timeframe            datetime alert price
1  RPMG3       15M 2018-01-03 16:40:00   buy  8.19
2  EMBR3       30M 2018-01-03 13:00:00  sell 21.02
3  CTKA4       15M 2018-01-02 14:18:00   buy  3.05
8 SPXI11       30M 2018-01-02 11:16:00  sell 90.47
```

It is possible to send to email with *mutt* or copy to *aws s3* bucket using the *alert* options on *config.yml*.

# Shiny App
A simple *Shiny* App showing charts of the wallet symbols, alerts triggered, etc.
```R
runShinyApp()
```

# Shell Scripts
There are some shell scripts calling various module functions on sh directory.

Live trading for all the symbols included on database.
```
sh/live.sh
```
