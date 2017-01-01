#!/bin/bash

Rscript -e "library('shiny')" -e "runApp(host = '$1', port = $2)" -e "warnings()"
