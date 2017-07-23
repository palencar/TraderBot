#!/bin/bash

Rscript -e "library('TraderBot')" -e "source('R/dbInterface.R')" -e "updateIntraday()"
