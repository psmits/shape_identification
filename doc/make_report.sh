#!/bin/bash
# compile the report

##Rscript -e "library(knitr); knit('turtle_report.Rnw')"

pdflatex turtle_report.tex
bibtex turtle_report.aux
pdflatex turtle_report.tex
pdflatex turtle_rerport.tex
pdflatex turtle_rerport.tex
