
library(data.table)
library(plyr)
library(reshape2)
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


tmp = seq(as.POSIXct('2013-01-01'),as.POSIXct('2014-12-31'), by="1 day")
tmp = data.frame(datetime=tmp,week=floor((as.POSIXlt(tmp)$yday+1)/7), month=as.POSIXlt(tmp)$mon+1)
Mode <- function(x) {
	ux <- unique(x)
	ux[which.max(tabulate(match(x, ux)))]
}
lookup = ddply(tmp,'week',function(df)Mode(df$month))
names(lookup) = c("week", "month")
mon.lookup = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG",
							 "SEP", "OCT", "NOV", "DEC")
lookup$month_name = mon.lookup[lookup$month]

# cutoffs = c(seq(1e5,5e6,1e5))
# corrs = rep(NA,0)
# 
# for(cutoff in cutoffs){

raw.slopes = fread('all.slopes.csv')
raw.slopes = raw.slopes[,rel.depth:=floor(10*depth/zmax)/10]
raw.slopes = raw.slopes[rel.depth <= 1,]

tiff('grl.figure.5.tiff', width=2400, height=2400, res=300, compression='lzw')
par(mfrow=c(2,2), mar=c(0.5,0.5,0,0), oma=c(7, 7, 2, 2))

a.cutoff = 3e5

################################################################################
## 1 START HERE -- TOP & BIG
################################################################################
all.slopes = raw.slopes[rel.depth<0.5 & area>a.cutoff,]


all.slopes = merge(all.slopes, lookup, by='week')

year.mon.slope = ddply(all.slopes,c('start','end','month_name'), function(df)median(df$slopes))
year.slope = ddply(all.slopes,c('start','end'), function(df)median(df$slopes))
year.slope$month_name = "YEAR"

this = rbind(year.mon.slope, year.slope)

names(this)[4] = 'wtr_slopes'
this$month = paste(this$month, 'wtr', sep='_')
finally = dcast(this, start+end~month, value.var='wtr_slopes')


wisco.air = fread('../supporting files/wisco.avg.airT.csv')
tmp = melt(wisco.air, 1)
u.mon = unique(tmp$variable)
output = data.frame()

for(i in 1:length(u.mon)){
	to.do = tmp[tmp$variable ==	u.mon[i],]
	res = all.slopes.perms(to.do$Year, to.do$value)
	
	res$month = to.do[1,]$variable
	output = rbind(output, res)
}
output$month = paste(output$month,'_air',sep='')

wisco.finally = dcast(output, start+end~month, value.var='slopes')


whew = merge(finally,wisco.finally, by=c('start','end'))


mon = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG",
							 "SEP", "OCT", "NOV", "DEC")
wtr.mon = paste(mon,'_wtr',sep='')
air.mon = paste(mon,'_air',sep='')

p.vals = matrix(NA, nrow=12, ncol=12)
colnames(p.vals) = air.mon
rownames(p.vals) = wtr.mon
for(i in 1:length(wtr.mon)){
	for(j in 1:length(air.mon)){
		if(j>i){next} #can't reverse causality
		
		if(!any(names(whew)%in%wtr.mon[i]) || !any(names(whew)%in%air.mon[j])){
			next
		}
		
		if(sum(complete.cases(whew[,c(wtr.mon[i],air.mon[j])])) < 50){
			next
		}
		
		try({p.vals[i,j] = cor.test(whew[,wtr.mon[i]], whew[,air.mon[j]], method='pearson')$statistic}, silent=TRUE)
	}
}
image(1:12, 1:12, p.vals[1:12,1:12], xaxt='n', yaxt='n',xlab='Wtr', ylab='Air', 
			col=tim.colors(8))
grid(nx=12, ny=12, lty=1)
#axis(1,at=1:12,labels=NA)
#axis(1,at=c(1,4,7,10),labels=mon[c(1,4,7,10)])
axis(2,at=0:12,labels=NA)
axis(2,at=c(1,4,7,10),labels=mon[c(1,4,7,10)])
text(1,12,"(a)")



################################################################################
## 2 START HERE -- TOP and SMALL
################################################################################
all.slopes = raw.slopes[rel.depth<0.5 & area<a.cutoff,]


all.slopes = merge(all.slopes, lookup, by='week')

year.mon.slope = ddply(all.slopes,c('start','end','month_name'), function(df)median(df$slopes))
year.slope = ddply(all.slopes,c('start','end'), function(df)median(df$slopes))
year.slope$month_name = "YEAR"

this = rbind(year.mon.slope, year.slope)

names(this)[4] = 'wtr_slopes'
this$month = paste(this$month, 'wtr', sep='_')
finally = dcast(this, start+end~month, value.var='wtr_slopes')


wisco.air = fread('../supporting files/wisco.avg.airT.csv')
tmp = melt(wisco.air, 1)
u.mon = unique(tmp$variable)
output = data.frame()

for(i in 1:length(u.mon)){
	to.do = tmp[tmp$variable ==	u.mon[i],]
	res = all.slopes.perms(to.do$Year, to.do$value)
	
	res$month = to.do[1,]$variable
	output = rbind(output, res)
}
output$month = paste(output$month,'_air',sep='')

wisco.finally = dcast(output, start+end~month, value.var='slopes')


whew = merge(finally,wisco.finally, by=c('start','end'))


mon = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG",
				"SEP", "OCT", "NOV", "DEC")
wtr.mon = paste(mon,'_wtr',sep='')
air.mon = paste(mon,'_air',sep='')

p.vals = matrix(NA, nrow=12, ncol=12)
colnames(p.vals) = air.mon
rownames(p.vals) = wtr.mon
for(i in 1:length(wtr.mon)){
	for(j in 1:length(air.mon)){
		if(j>i){next} #can't reverse causality
		
		if(!any(names(whew)%in%wtr.mon[i]) || !any(names(whew)%in%air.mon[j])){
			next
		}
		
		if(sum(complete.cases(whew[,c(wtr.mon[i],air.mon[j])])) < 50){
			next
		}
		
		try({p.vals[i,j] = cor.test(whew[,wtr.mon[i]], whew[,air.mon[j]], method='pearson')$statistic}, silent=TRUE)
	}
}
image(1:12, 1:12, p.vals[1:12,1:12], xaxt='n', yaxt='n',xlab='Wtr', ylab='Air', 
      col=tim.colors(8))
grid(nx=12, ny=12, lty=1)
#axis(1,at=1:12,labels=NA)
#axis(1,at=c(1,4,7,10),labels=mon[c(1,4,7,10)])
#axis(2,at=1:12,labels=NA)
#axis(2,at=c(1,4,7,10),labels=mon[c(1,4,7,10)])
text(1,12,"(b)")


################################################################################
## 3 START HERE --BOTTOM and BIG
################################################################################
all.slopes = raw.slopes[rel.depth>=0.5 & area>a.cutoff,]


all.slopes = merge(all.slopes, lookup, by='week')

year.mon.slope = ddply(all.slopes,c('start','end','month_name'), function(df)median(df$slopes))
year.slope = ddply(all.slopes,c('start','end'), function(df)median(df$slopes))
year.slope$month_name = "YEAR"

this = rbind(year.mon.slope, year.slope)

names(this)[4] = 'wtr_slopes'
this$month = paste(this$month, 'wtr', sep='_')
finally = dcast(this, start+end~month, value.var='wtr_slopes')


wisco.air = fread('../supporting files/wisco.avg.airT.csv')
tmp = melt(wisco.air, 1)
u.mon = unique(tmp$variable)
output = data.frame()

for(i in 1:length(u.mon)){
	to.do = tmp[tmp$variable ==	u.mon[i],]
	res = all.slopes.perms(to.do$Year, to.do$value)
	
	res$month = to.do[1,]$variable
	output = rbind(output, res)
}
output$month = paste(output$month,'_air',sep='')

wisco.finally = dcast(output, start+end~month, value.var='slopes')


whew = merge(finally,wisco.finally, by=c('start','end'))


mon = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG",
				"SEP", "OCT", "NOV", "DEC")
wtr.mon = paste(mon,'_wtr',sep='')
air.mon = paste(mon,'_air',sep='')

p.vals = matrix(NA, nrow=12, ncol=12)
colnames(p.vals) = air.mon
rownames(p.vals) = wtr.mon
for(i in 1:length(wtr.mon)){
	for(j in 1:length(air.mon)){
		if(j>i){next} #can't reverse causality
		
		if(!any(names(whew)%in%wtr.mon[i]) || !any(names(whew)%in%air.mon[j])){
			next
		}
		
		if(sum(complete.cases(whew[,c(wtr.mon[i],air.mon[j])])) < 50){
			next
		}
		
		try({p.vals[i,j] = cor.test(whew[,wtr.mon[i]], whew[,air.mon[j]], method='pearson')$statistic}, silent=TRUE)
	}
}
image(1:12, 1:12, p.vals[1:12,1:12], xaxt='n', yaxt='n',xlab='Wtr', ylab='Air', col=tim.colors(8))
grid(nx=12, ny=12, lty=1)
axis(1,at=1:12,labels=NA)
axis(1,at=c(1,4,7,10),labels=mon[c(1,4,7,10)])
axis(2,at=0:12,labels=NA)
axis(2,at=c(1,4,7,10),labels=mon[c(1,4,7,10)])
text(1,12,"(c)")


################################################################################
## 4 START HERE --SMALL and BOTTOM
################################################################################
all.slopes = raw.slopes[rel.depth>=0.5 & area<a.cutoff,]


all.slopes = merge(all.slopes, lookup, by='week')

year.mon.slope = ddply(all.slopes,c('start','end','month_name'), function(df)median(df$slopes))
year.slope = ddply(all.slopes,c('start','end'), function(df)median(df$slopes))
year.slope$month_name = "YEAR"

this = rbind(year.mon.slope, year.slope)

names(this)[4] = 'wtr_slopes'
this$month = paste(this$month, 'wtr', sep='_')
finally = dcast(this, start+end~month, value.var='wtr_slopes')


wisco.air = fread('../supporting files/wisco.avg.airT.csv')
tmp = melt(wisco.air, 1)
u.mon = unique(tmp$variable)
output = data.frame()

for(i in 1:length(u.mon)){
	to.do = tmp[tmp$variable ==	u.mon[i],]
	res = all.slopes.perms(to.do$Year, to.do$value)
	
	res$month = to.do[1,]$variable
	output = rbind(output, res)
}
output$month = paste(output$month,'_air',sep='')

wisco.finally = dcast(output, start+end~month, value.var='slopes')


whew = merge(finally,wisco.finally, by=c('start','end'))


mon = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG",
				"SEP", "OCT", "NOV", "DEC")
wtr.mon = paste(mon,'_wtr',sep='')
air.mon = paste(mon,'_air',sep='')

p.vals = matrix(NA, nrow=12, ncol=12)
colnames(p.vals) = air.mon
rownames(p.vals) = wtr.mon
for(i in 1:length(wtr.mon)){
	for(j in 1:length(air.mon)){
		if(j>i){next} #can't reverse causality
		
		if(!any(names(whew)%in%wtr.mon[i]) || !any(names(whew)%in%air.mon[j])){
			next
		}
		
		if(sum(complete.cases(whew[,c(wtr.mon[i],air.mon[j])])) < 50){
			next
		}
		
		try({p.vals[i,j] = cor.test(whew[,wtr.mon[i]], whew[,air.mon[j]], method='pearson')$statistic}, silent=TRUE)
	}
}
image(1:12, 1:12, p.vals[1:12,1:12], xaxt='n', yaxt='n',xlab='Wtr', ylab='Air', col=tim.colors(8))
grid(nx=12, ny=12, lty=1)
axis(1,at=0:12,labels=NA)
axis(1,at=c(1,4,7,10),labels=mon[c(1,4,7,10)])
#axis(2,at=1:12,labels=NA)
#axis(2,at=c(1,4,7,10),labels=mon[c(1,4,7,10)])
text(1,12,"(d)")

mtext(expression(Large~(Area~">"~0.3~km^2)), side = 3, outer = TRUE, line=0.5, cex=1, adj=0.23)
mtext(expression(Small(Area~"<"~0.3~km^2)), side = 3, outer = TRUE, line=0.5, cex=1, adj=0.79)

mtext(expression(Top~(rel.depth~"<"~0.5)), side=2, outer = TRUE, line=2, cex=1, adj=0.79)
mtext(expression(Bottom~(rel.depth~">"~0.5)), side=2, outer = TRUE, line=2, cex=1, adj=0.23)

mtext("Air Temperature Signal", side=2, outer=TRUE, line=4, cex=1.5)
mtext("Water Temperature Signal", side=1, outer=TRUE, line=4, cex=1.5)

dev.off()