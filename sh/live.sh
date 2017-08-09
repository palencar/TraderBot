#!/bin/bash

Rscript -e "library('TraderBot')" -e "library('config')" -e "config <- config::get()" -e "computeStream(timeFrame = '$1', updateData = FALSE)" -e "warnings()"
