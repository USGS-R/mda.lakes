## Figure Stratified slope estimation
library(plyr)
library(data.table)
library(rLakeAnalyzer)
source('../R/Libraries/GLM.functions.R')

all.slopes.perms = function(times, data){
	
	if(length(data)==2){
		perm.i = matrix(c(1,2))
		#return((data[1]-data[2])/(times[1]-times[2]))
	}else{
		perm.i = combn(1:length(data),2)
	}
	
	
	#perm1 = data[perm.i[1,],]
	#perm2 = data[perm.i[2,],]
	starts = apply(matrix(times[c(perm.i[1,], perm.i[2,])], ncol=2), 1, min)
	ends   = apply(matrix(times[c(perm.i[1,], perm.i[2,])], ncol=2), 1, max)
	dts    = abs(times[perm.i[1,]] - times[perm.i[2,]])
	
	slopes = (data[perm.i[1,]]-data[perm.i[2,]])/(times[perm.i[1,]] - times[perm.i[2,]])
	
	output = data.frame(slopes, start=starts, end=ends, dt=diff(range(times)), n.obs=length(times))
	return(output)
}

wtr = fread('../supporting files/wtemp.obs.tsv')
setnames(wtr, names(wtr), tolower(names(wtr)))
setkey(wtr, wbic)
wtr[,wtemp:=water.density(wtemp)]##Just now

wtr[, datetime:=as.POSIXct(datetime)]
wtr[, month:=as.POSIXlt(datetime)$mon+1]
wtr[, doy:=  as.POSIXlt(datetime)$yday+1]
wtr[, week:= floor((as.POSIXlt(datetime)$yday+1)/7)]
wtr[, year:=as.POSIXlt(datetime)$year+1900]

#wtr = ddply(wtr, c('wbic', 'datetime'), function(df) df[which.min(df$depth),])
#wtr = data.table(wtr)
wtr[,depth:=ceiling(depth)] #set all depths to the deeper whole meter

wtr = wtr[year > 1979]

setkey(wtr, wbic, week, depth)
u.events = unique(wtr[,list(wbic, week, depth)])

#setkey(wtr, wbic, datetime, depth)


all.slopes = data.table()
n.vals = rep(NA, nrow(u.events))
n = 0
for(i in 1:nrow(u.events)){
	
	if(i%%100 == 0){
		cat(100*i/nrow(u.events), 'percent done\n')
	}
	
	u.event = u.events[i,]
	
	moment = wtr[wbic==u.event$wbic & week==u.event$week & depth==u.event$depth, ]
	
	
	if(nrow(moment) < 2){
		next
	}
	n.vals[i] = nrow(moment)

	
	slopes = all.slopes.perms(moment[,year], moment[,wtemp])
	slopes$wbic = u.event$wbic
	
	all.slopes = rbind(all.slopes, merge(u.event, slopes, by='wbic'))
	n = n + nrow(moment)
}

all.slopes = all.slopes[!is.infinite(slopes) & !is.na(slopes),]

u.wbic = unique(all.slopes$wbic)
areas  = rep(NA, length(u.wbic))
kds    = rep(NA, length(u.wbic))

for(i in 1:length(u.wbic)){
	area = getArea(as.character(u.wbic[i]))
	if(!is.null(area)){
		areas[i] = area
	}
	kds[i] = getClarity(as.character(u.wbic[i]))
}

all.slopes = merge(all.slopes, data.frame(wbic=u.wbic, area=areas, kd=kds), by='wbic')

u.wbic = unique(all.slopes$wbic)
zmaxes = rep(NA,length(u.wbic))
for(i in 1:length(u.wbic)){
	zmax = getZmax(as.character(u.wbic[i]))
	if(!is.null(zmax)){
		zmaxes[i] = zmax
	}
}
all.slopes = merge(all.slopes, data.frame(wbic=u.wbic, zmax=zmaxes), by='wbic')

write.table(all.slopes,'all.slopes.density.csv', row.names=FALSE, sep=',')
#write.table(n.rows, 'all.slopes.n.csv', row.names=FALSE, sep=',')


