#!/bin/bash

Rscript -e "library('TraderBot')" -e "computeBacktest('$1', printCharts = FALSE, minSamples = 100000)" -e "warnings()"
