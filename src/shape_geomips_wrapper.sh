#!/bin/bash

nohup nice R CMD BATCH --vanilla turtle_machine_learning.r &

echo 'everything is ok' | mail -s 'geomips' psmits@uchicago.edu
