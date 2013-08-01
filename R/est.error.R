# load lake summary document, which holds WBIC and SE

source('GLM.functions.R')

dat <-	read.table(file=fileName,header=TRUE,sep='\t')
n.cut 	<- 20	# data points required for analysis
# remove NAs
WBIC	<-	dat$WBIC
SE	<-	dat$StndErr
n	<-	dat$numPoints

rmvI	<-	(is.na(SE) | n < n.cut)
WBIC	<-	WBIC[!rmvI]
n	<-	n[!rmvI]
SE	<-	SE[!rmvI]
n.lakes	<-	length(SE)

area	<-	vector(length=n.lakes)
SDF	<-	vector(length=n.lakes)
depth	<-	vector(length=n.lakes)
res.time	<-	vector(length=n.lakes)
canopy	<-	vector(length=n.lakes)
clarity	<-	vector(length=n.lakes)

for (i in 1:n.lakes){
	area[i]	<-	getArea(WBIC[i])
	SDF[i]	<-	getSDF(WBIC[i])
}

