#!/bin/bash

Rscript -e "library('TraderBot')" -e "chartList('$1', dev='png')" -e "warnings()"
