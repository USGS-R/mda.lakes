# load lake summary document, which holds WBIC and SE

source('GLM.functions.R')
fileName	<-	'../supporting files/summary.txt'
dat <-	read.table(file=fileName,header=TRUE,sep='\t')
n.cut 	<- 20	# data points required for analysis
# remove NAs
WBICs	<-	as.character(dat$WBIC)
SE	<-	dat$StndErr
n	<-	dat$numPoints


rmvI	<-	(is.na(SE) | n < n.cut)
WBICs	<-	WBICs[!rmvI]
n	<-	n[!rmvI]
SE	<-	SE[!rmvI]
n.lakes	<-	length(SE)

hist(SE,breaks=seq(0,8,.25))
print(median(SE))
area	<-	vector(length=n.lakes)
SDF	<-	vector(length=n.lakes)
depth	<-	vector(length=n.lakes)
res.time	<-	vector(length=n.lakes)
canopy	<-	vector(length=n.lakes)
clarity	<-	vector(length=n.lakes)
perim	<-	vector(length=n.lakes)
Wstr	<-	vector(length=n.lakes)

for (i in 1:n.lakes){
	area[i]	<-	getArea(WBICs[i])
	perim[i]	<-	getPerim(WBICs[i])
	SDF[i]	<-	getSDF(WBICs[i])
	depth[i]	<-	getZmax(WBICs[i])
	#res.time[i]
	canopy[i]	<- getCanopy(WBICs[i])
	Wstr[i]	<-	getWstr(WBICs[i])
	clarity[i]	<-	getClarity(WBICs[i])
}

perSq.area	<-	sqrt(perim)/area
lm	<-	lm(SE~perSq.area-1)

plot(SE,perSq.area*17541.3)

summary(lm)
#plot(lm)