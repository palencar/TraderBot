#!/bin/bash

Rscript -e "library('TraderBot')" -e "computeBacktest('$1', printCharts = FALSE, samples = 3)" -e "warnings()"
