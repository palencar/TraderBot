#!/bin/bash

Rscript -e "library('TraderBot')" -e "computeSimulation(startDate=as.Date('$1'), endDate=as.Date('$2'))" -e "warnings()"
