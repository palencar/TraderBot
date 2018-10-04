#!/bin/bash

Rscript -e "library('TraderBot')" -e "library('config')" -e "config <- config::get()" -e "Sys.setenv(TZ='America/Sao_Paulo')" -e "computeStream(timeFrames = c('5M', '10M', '15M', '30M', '1H', '1D'))" -e "warnings()"
