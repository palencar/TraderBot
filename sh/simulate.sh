#!/bin/bash

Rscript -e "library('TraderBot')" -e "computeSimulation(timeFrame = '$1')" -e "warnings()"
