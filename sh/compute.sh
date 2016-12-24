#!/bin/bash

Rscript -e "library('TraderBot')" -e "computeBacktest('$1', FALSE)" -e "warnings()"
