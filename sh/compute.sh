#!/bin/bash

Rscript -e "library('TraderBot')" -e "computeBacktest('$1', '2009-01-01', Sys.Date(), TRUE)" -e "warnings()"
