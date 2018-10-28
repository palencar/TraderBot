#!/bin/bash

Rscript -e "Sys.setenv(TZ='America/Sao_Paulo')" -e "library('TraderBot')" -e "computeSimulation(timeFrame = '$1')" -e "warnings()"
