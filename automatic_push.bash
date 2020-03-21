#!/bin/bash

# Author
# Purpose
# Date_creation
# Date_modification

data=(date + "%m-%d-%Y %H:%M")

$data

git pull
Rscript -e "rmarkdown::render('index.Rmd')"
git add .
git commit -m "automatic update $data"
