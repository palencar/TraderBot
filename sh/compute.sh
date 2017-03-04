#!/bin/bash

Rscript -e "library('TraderBot')" -e "computeBacktest('$1', printCharts = FALSE, minSamples = 1048576)" -e "warnings()"
