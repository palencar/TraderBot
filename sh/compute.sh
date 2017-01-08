#!/bin/bash

Rscript -e "library('TraderBot')" -e "computeBacktest('$1', printCharts = FALSE)" -e "warnings()"
