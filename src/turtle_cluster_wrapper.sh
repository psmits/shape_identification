#!/bin/bash

nohup nice R CMD BATCH --vanilla turtle_cluster.r &

echo 'everything is ok' | mail -s 'geomips' psmits@uchicago.edu
