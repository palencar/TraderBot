#!/bin/bash

Rscript -e "library('TraderBot')" -e "computeSimulation('$1', '$2', '$3', FALSE)" -e "warnings()"
