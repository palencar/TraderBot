#!/bin/bash

Rscript -e "Sys.setenv(TZ='America/Sao_Paulo')" -e "library('TraderBot')" -e "runShinyApp(options = list(host='127.0.0.1', port=8000))" -e "warnings()"
