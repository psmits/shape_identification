#!/bin/bash

nohup nice R CMD BATCH --vanilla turtle_supervised_data.r
nohup nice R CMD BATCH --vanilla turtle_logistic.r
nohup nice R CMD BATCH --vanilla turtle_nnet.r
nohup nice R CMD BATCH --vanilla turtle_rf.r
nohup nice R CMD BATCH --vanilla turtle_analysis.r

echo 'analysis complete' | mail -s 'turtle shape' psmits@uchiago.edu
