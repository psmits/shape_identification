#!/bin/bash

cd ./R

nohup nice R CMD BATCH --vanilla ../R/generalize.r &
nohup nice R CMD BATCH --vanilla ../R/shape_analysis.r &
nohup nice R CMD BATCH --vanilla ../R/make_plots &
nohup nice R CMD BATCH --vanilla ../R/make_tables &

cd ..

echo 'analysis complete' | mail -s 'turt' psmits@uchicago.edu
