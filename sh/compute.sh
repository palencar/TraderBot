#!/bin/bash

Rscript -e "library('TraderBot')" -e "computeBacktest('$1', minSamples = $2, timeFrame = '$3', replaceFile = TRUE)" -e "warnings()"

