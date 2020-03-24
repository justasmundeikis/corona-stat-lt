#!/bin/bash

# Author
# Purpose
# Date_creation
# Date_modification

data=$(date +"%m-%d-%Y %H:%M");
echo "started $(date)">> /home/pi/list_log.txt;
echo $(pwd) >>/home/pi/list_log.txt;

git pull;
Rscript -e "rmarkdown::render('index.Rmd')";
git add .;
git commit -m "automatic update $data";
git push origin master;

echo "finished $(date)">> /home/pi/list_log.txt
