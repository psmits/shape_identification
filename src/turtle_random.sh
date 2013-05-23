#!/bin/bash

nohup nice R CMD BATCH --vanilla turtle_random_rf.r
nohup nice R CMD BATCH --vanilla turtle_random_log.r

echo 'analysis complete' | mail -s 'turtle shape' psmits@uchiago.edu
