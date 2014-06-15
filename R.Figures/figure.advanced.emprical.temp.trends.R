## Figure Stratified slope estimation
library(plyr)
library(data.table)


all.slopes.perms = function(times, data){
	
	if(length(data)==2){
		return((data[1]-data[2])/(times[1]-times[2]))
	}
	
	perm.i = combn(1:length(data),2)
	#perm1 = data[perm.i[1,],]
	#perm2 = data[perm.i[2,],]
	#browser()
	
	return((data[perm.i[1,]]-data[perm.i[2,]])/(times[perm.i[1,]] - times[perm.i[2,]]))
}

wtr = fread('../supporting files/wtemp.obs.tsv')
setnames(wtr, names(wtr), tolower(names(wtr)))
setkey(wtr, wbic)

wtr[, datetime:=as.POSIXct(datetime)]
wtr[, month:=as.POSIXlt(datetime)$mon+1]
wtr[, doy:=  as.POSIXlt(datetime)$yday+1]
wtr[, week:= floor((as.POSIXlt(datetime)$yday+1)/7)]
wtr[, year:=as.POSIXlt(datetime)$year+1900]

#wtr = ddply(wtr, c('wbic', 'datetime'), function(df) df[which.min(df$depth),])
#wtr = data.table(wtr)
wtr[,depth:=ceiling(depth)] #set all depths to the deeper whole meter

setkey(wtr, wbic, week, depth)
u.events = unique(wtr[,list(wbic, week, depth)])

#setkey(wtr, wbic, datetime, depth)


all.slopes = data.table()
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
	
	slopes = all.slopes.perms(moment[,year], moment[,wtemp])
	
	all.slopes = rbind(all.slopes, 
										 merge(u.event,data.table(slopes=slopes, wbic=u.event$wbic), by='wbic'))
	n = n + nrow(moment)
}

all.slopes = all.slopes[!is.infinite(slopes) & !is.na(slopes),]

write.table(all.slopes,'all.slopes.csv', row.names=FALSE, sep=',')


