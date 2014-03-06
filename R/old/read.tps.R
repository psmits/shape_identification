#####################################################################
#
#  Creates a function called read.tps() that reads a TPS format
#  morphometrics data file that takes POINT and CURVE imputs and 
#  converts it to the form required by Ian Dryden's shape libary.  
#  Written by P. David Polly, version 1.0, 2008.  
#  current version 1.1, 2010.
#  modified by A. Michelle Lawing 2010
#
#####################################################################

read.tps <- function(filename) {

tps <- file(filename,"r")
contents <- readLines(tps)
close(tps)
nmSpecs <- 0
nmLands <- as.numeric(substr(contents[1],4,1000))
tmpLabels <- vector(mode="character",length=0)
a=1
i=1
while(a==1){
if(substr(contents[i],1,7)=="CURVES=") {
nmCurves<-as.numeric(substr(contents[i],8,1000))
a=a+1
}
i=i+1
if(i==length(contents)){
a=a+1
nmCurves<-0
}
}
if(nmCurves!=0){
curves<-array(dim=nmCurves)
a=1
i=1
while(a<nmCurves+1){
if(substr(contents[i],1,7)=="POINTS=") {
curves[a]<-as.numeric(substr(contents[i],8,1000))
a=a+1
}
i=i+1
}
}
k<-nmLands
if(nmCurves!=0){
k<-nmLands+sum(curves)

}
for(i in 1:length(contents)) {
if(substr(contents[i],1,3)=="ID=") {
tmpLabels <- append(tmpLabels,substr(contents[i],4,100))
}
if(substr(contents[i],1,3)=="LM=") {
nmSpecs <- nmSpecs + 1
}
}
lenRecord <- length(contents)/nmSpecs
posLands <- rep(c(FALSE,rep(TRUE,nmLands),rep(FALSE,lenRecord-nmLands-1)),nmSpecs)
if(nmCurves!=0){
a<-1
i<-1
for(j in 1:nmSpecs){
while(a<nmCurves+1){
if(substr(contents[i],1,7)=="POINTS=") {
posLands[(i+1):(i+curves[a])]<-TRUE
a<-a+1
}
i<-i+1
}
a<-1
}
}

landLines <- contents[posLands]
tmpLands <- vector(mode="numeric",length=0)
for(k in 1:length(landLines)) {
tmp <- strsplit(landLines[k]," ")
tmpLands <- append(tmpLands,as.numeric(tmp[[1]]))
}
tpsContents <- array(tmpLands,dim=c(length(tmp[[1]]),(nmLands+sum(curves)),nmSpecs))
tpsContents <- aperm(tpsContents,c(2,1,3))

for(j in 1:nmSpecs){
scale<-as.numeric(substr(contents[(j*lenRecord)],8,1000))
tpsContents[,,j]<-tpsContents[,,j]*scale
}
return(tpsContents)
}
