#!/bin/bash

Rscript -e "library('TraderBot')" -e "saveSymbols('$1')" -e "warnings()"
