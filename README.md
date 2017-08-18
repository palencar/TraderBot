# TraderBot

# Installation

## Ubuntu Dependencies
```
apt-get install -y r-base r-base-dev sqlite3 libxml2-dev libmysqlclient-dev libssl-dev libssh2-1-dev libcurl4-openssl-dev
```

## Clone repository
Clone the git repository and start a R session.
```
git clone https://github.com/palencar/TraderBot
cd TraderBot
```

## SQLite Database
Create the database from the schema file.
```
sqlite3 db.sqlite < sqlite3-schema.sql
```

Uncoment the following lines:
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

Install TraderBot and dependencies
```R
library(devtools)
install_git(".")
```

# Using

## Load TraderBot
```R
library(TraderBot)
```

## Donwload Data
Download daily data from google.
```R
saveSymbolsDaily('BVMF3')
```

Download last *15* days of intraday data from google.
```R
saveSymbolsIntraday('BVMF3')
```

## Backtest
Test strategy for *minSamples* random parameters.
```R
computeBacktest('BVMF3', minSamples = 10)
```

## Simulate
Test strategy using the values on *config.yml* file.
```R
computeSimulation('BVMF3')
```

## Live Trading
Update assets on database and execute the strategy. The parameter *timeFrame* can be one of the following:
1D, 1H, 30M, 15M, 10M and 5M.
```R
computeStream(timeFrame = '30M')
```

## Shiny App
A simple *Shiny* app showing the backtest results, alerts triggered on the live trading and a chart tool.
```R
runShinyApp()
```

## Shell Scripts
There are some shell scripts calling various module functions on sh directory.

Live trading on *30M* *time frame*.
```
sh/live.sh 30M
```
