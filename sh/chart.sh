#!/bin/bash

Rscript -e "library('TraderBot')" -e "chartList('$1')"" -e "warnings()"
