#!/bin/bash

Rscript -e "library('TraderBot')" -e "library('config')" -e "config <- config::get()" -e "computeStream(timeFrames = c('15M', '30M', '1H', '1D'))" -e "warnings()"
