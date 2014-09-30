#Lets look at some buoy data

library(rLakeAnalyzer)
library(stringr)
library(data.table)
library(plyr)
Sys.setenv(TZ='GMT')

wtr.line.plot = function(to.plot){
	plot(to.plot[,1], to.plot[,2], type='l')
	
	for(i in 3:ncol(to.plot)){
		lines(to.plot[,1], to.plot[,i])
	}
}


data.dir = 'd:/Dropbox/Work/Clean Buoy Data'
lakes = c('Mendota', 'Sparkling', 'troutlake')


all.output = data.frame()

for(i in 1:length(lakes)){
	l = lakes[i]
	
	wtr.files = Sys.glob(file.path(data.dir, l, '*','*.wtr'))
	years = as.numeric(str_extract(wtr.files,'\\d+'))
	
	output = data.frame(year=years, lake=l)
	
	for(j in 1:nrow(output)){
		
		d = load.ts(wtr.files[j])
		d = d[,sapply(d,function(s)!all(is.na(s)))]
		d = data.table(d)
		
		d[,month:=as.POSIXlt(datetime)$mon+1]
		d[,hour:=as.POSIXlt(datetime)$hour]
		d[,doy:=as.POSIXlt(datetime)$yday+1]
		setkey(d, month, hour, doy)
		
		d = d[month >=7 & month <=9,]
		
		top.name = names(d)[2]
		
		output$JAS.hires[j] = mean(d[,top.name, with=FALSE][[1]], na.rm=TRUE)
		
		output$JAS.nights[j] = mean(d[hour==0,top.name, with=FALSE][[1]], na.rm=TRUE)
			
		output$JAS.days[j] = mean(d[hour==12,top.name, with=FALSE][[1]], na.rm=TRUE)
		
		tmp = data.frame(d[,lapply(.SD, mean, na.rm=TRUE), by=doy])
		
		output$JAS.daily[j] = mean(tmp[, top.name], na.rm=TRUE)		

	}
	all.output = rbind(all.output,output)
}
all.output = merge(all.output, 
									 data.frame(lake=c('Mendota', 'Sparkling','troutlake'), wbic=c(805400, 1881900, 2331600)), by='lake')
all.output = data.table(all.output)




better.JAS = function(df){
	if(nrow(df) > 3){
		return(mean(df$V1))
	}else{
		return(NA)
	}
}

wtr = read.table('../supporting files/wtemp.obs.tsv', sep='\t', header=TRUE)
lter.wbic = rbind(read.table('../supporting files/lter.south.lakes.tsv', header=TRUE, sep='\t'), 
									read.table('../supporting files/lter.north.lakes.tsv', header=TRUE, sep='\t'))
wtr = wtr[wtr$WBIC %in% lter.wbic$WBIC, ]
wtr.near.surf = ddply(wtr, c("WBIC", "DATETIME"), function(df) mean(df$WTEMP[df$DEPTH<=3], na.rm=TRUE))

wtr.near.surf$DATETIME = as.POSIXct(wtr.near.surf$DATETIME)
wtr.near.surf = wtr.near.surf[!is.na(wtr.near.surf$V1), ]

quick.interp = function(df){
	newx = seq(min(df$DATETIME), max(df$DATETIME), by='days')
	newy = approx(df$DATETIME, df$V1, newx, method='linear')$y
	return(data.frame(DATETIME=newx, V1=newy))
}

wtr.surf.interp = ddply(wtr.near.surf, c('WBIC'), quick.interp)
wtr.near.surf = wtr.surf.interp

mons = as.POSIXlt(wtr.near.surf$DATETIME)$mon+1

wtr.near.surf = wtr.near.surf[mons >=7 & mons <= 9,]
wtr.near.surf$year = as.POSIXlt(wtr.near.surf$DATETIME)$year+1900

JAS.mean = ddply(wtr.near.surf, c("WBIC", "year"), better.JAS)
JAS.mean = JAS.mean[!is.na(JAS.mean$V1),]
names(JAS.mean) = c('wbic', 'year', 'jas.lter')

all.jas = merge(all.output, JAS.mean, by=c('wbic', 'year'))

sp.output = all.jas[all.jas$lake=='Sparkling',]

tiff('../Figures/jas.interp.sp.buoy.vs.lter.tiff', width=1600, height=1200, res=300, compression='lzw')
plot(sp.output$year, sp.output$JAS.hires, type='b')
lines(sp.output$year, sp.output$JAS.nights, type='b')
lines(sp.output$year, sp.output$JAS.days, type='b')
lines(sp.output$year, sp.output$JAS.daily, type='b')
lines(sp.output$year, sp.output$jas.lter, lwd=2, col='red')
dev.off()

me.output = all.jas[all.jas$lake=='Mendota',]
tiff('../Figures/jas.interp.me.buoy.vs.lter.tiff', width=1600, height=1200, res=300, compression='lzw')
plot(me.output$year, me.output$JAS.hires, type='b', ylim=c(21,26))
lines(me.output$year, me.output$JAS.nights, type='b')
lines(me.output$year, me.output$JAS.days, type='b')
lines(me.output$year, me.output$JAS.daily, type='b')
lines(me.output$year, me.output$jas.lter, lwd=2, col='red')
dev.off()

tr.output = all.jas[all.jas$lake=='troutlake',]
tiff('../Figures/jas.interp.tr.buoy.vs.lter.tiff', width=1600, height=1200, res=300, compression='lzw')
plot(tr.output$year, tr.output$JAS.hires, type='b', ylim=c(18,23))
lines(tr.output$year, tr.output$JAS.nights, type='b')
lines(tr.output$year, tr.output$JAS.days, type='b')
lines(tr.output$year, tr.output$JAS.daily, type='b')
lines(tr.output$year, tr.output$jas.lter, lwd=2, col='red')
dev.off()

