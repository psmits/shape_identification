library(knitcitations)

libs <- list('RCore' = citation(),
             'Dryden2013' = citation('shapes'),
             'Kuhn2013' = citation('caret'),
             'Venables2002' = citation('nnet'),
             'Liaw2002' = citation('randomForest'),
             'Maechler2013' = citation('cluster'))

write.bibtex(libs, file = '../documents/packages.bib')
