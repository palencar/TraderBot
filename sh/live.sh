#!/bin/bash

Rscript -e "library('TraderBot')" -e "computeStream(timeFrame = '$1')" -e "warnings()"
