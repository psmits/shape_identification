#!/bin/bash

nohup nice R CMD BATCH --vanilla turtle_supervised_data.r
nohup nice R CMD BATCH --vanilla turtle_logistic.r
nohup nice R CMD BATCH --vanilla turtle_rf.r
nohup nice R CMD BATCH --vanilla turtle_lda.r
nohup nice R CMD BATCH --vanilla turtle_analysis.r
nohup nice R CMD BATCH --vanilla turtle_generalize.r

echo 'analysis complete' | mail -s 'turtle shape' psmits@uchiago.edu
