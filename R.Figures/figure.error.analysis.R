## Let's look at residuals and how they look over time.
library(plyr)
library(zyp)
library(data.table)
library(akima)
library(grid)

all.cal = fread('D:/WilmaRuns/2014-05-08multipliers/v1.2lwMultip1/all.cal.tsv')
#lter.wbic = read.table('../supporting files/lter.south.lakes.tsv', header=TRUE, sep='\t')

#lter.wbic = rbind(read.table('../supporting files/lter.south.lakes.tsv', header=TRUE, sep='\t'),
#									read.table('../supporting files/lter.north.lakes.tsv', header=TRUE, sep='\t'))

#all.cal = all.cal[all.cal$WBIC %in% lter.wbic$WBIC, ]

#surf.cal = all.cal[all.cal$DEPTH <= 3, ]

all.cal$DATETIME = as.POSIXct(all.cal$DATETIME)
all.cal$RESID = all.cal$WTEMP-all.cal$WTEMP_MOD

all.cal = all.cal[all.cal$RESID < 100, ] #there's like one wild flier

all.cal = all.cal[!is.na(all.cal$RESID), ]

tmp.dates = as.POSIXlt(all.cal$DATETIME)
all.cal$YEAR = tmp.dates$year + 1900
all.cal$DOY = tmp.dates$yday

#all.cal = all.cal[all.cal$YEAR > 1995,]

rmse = function(data, ...){
	return(sqrt(mean(data^2, ...)))
}

################################################################################
## Slopes all Resids
################################################################################

num.dates = as.numeric(all.cal$DATETIME)/(365*60*60*24)
resids = all.cal$RESID

lm(resids~num.dates) #increasing

abs.resids = abs(resids)
lm(abs.resids~num.dates) #increasing

sqr.resid = resids^2
lm(sqr.resid~num.dates) #increasing

################################################################################
## All Resids with median resids through time
################################################################################

yearly.rmse = ddply(all.cal, 'YEAR', function(df) rmse(df$RESID, na.rm=TRUE))
yearly.median.resids = ddply(all.cal, 'YEAR', function(df) median(df$RESID, na.rm=TRUE))
yearly.mean.resids = ddply(all.cal, 'YEAR', function(df) mean(df$RESID, na.rm=TRUE))

tiff('../Figures/error.all.resids.tiff', width=1600, height=1600, res=300, compression='lzw')
plot(all.cal$DATETIME, all.cal$RESID, ylab='Residual (obs-mod)', xlab='Year')
lines(as.POSIXct(paste(yearly.median.resids$YEAR, '-06-15', sep='')),
			yearly.median.resids[,2], lwd=2, col='red')

num.dates = as.numeric(all.cal$DATETIME)
resids = all.cal$RESID
abline(lm(resids~num.dates), col='blue')

num.dates = as.numeric(all.cal$DATETIME)/(365*60*60*24)
slope = lm(resids~num.dates)$coeff[2]
grid.text(paste("Slope:",slope, 'C/yr'), x=unit(2, "mm"), y=unit(1, "npc") - unit(2, "mm"), just=c("left", "top")) 
dev.off()

tiff('../Figures/error.median.resids.tiff', width=1600, height=1600, res=300, compression='lzw')
plot(yearly.median.resids$YEAR, yearly.median.resids[,2],ylab='Median Residual (obs-mod)', xlab='Year')
#abline(a=median(yearly.median.resids[,2]), b=0, lwd=2)
abline(lm(yearly.median.resids[,2] ~ yearly.median.resids[,1]))
dev.off()

lm(V1 ~ YEAR, yearly.rmse)
zyp.sen(V1 ~ YEAR, yearly.rmse)

lm(V1 ~ YEAR, yearly.median.resids)
zyp.sen(V1 ~ YEAR, yearly.median.resids)

lm(V1 ~ YEAR, yearly.mean.resids)
zyp.sen(V1 ~ YEAR, yearly.mean.resids)

################################################################################
## Are resids biased through the year? DOY versus resid
################################################################################

daily.median.resids = ddply(all.cal, 'DOY', function(df) median(df$RESID, na.rm=TRUE))

tiff('../Figures/error.resids.through.year.tiff', width=1600, height=1600, res=300, compression='lzw')
plot(all.cal$DOY, all.cal$RESID, xlab="Day of Year", ylab="Residual (obs-mod)")

lines(daily.median.resids[,1], daily.median.resids[,2], lwd=2, col='red')
legend('topright', legend=c('All Residuals', 'Daily Median Residual'), 
			 lty=c(0,1), pch=c(1,NA), col=c('black', 'red'), lwd=c(1,2))
dev.off()

tiff('../Figures/error.median.resids.through.year.tiff', width=1600, height=1600, res=300, compression='lzw')
plot(daily.median.resids[,1], daily.median.resids[,2], lwd=2, col='red')

abline(h=0, lwd=2)

dev.off()


################################################################################
## Hmmm, how about residuals of surface water in JAS alone?
################################################################################

surf.cal = all.cal[all.cal$DEPTH <= 3, ]
mons = as.POSIXlt(surf.cal$DATETIME)$mon+1

jas.surf.cal = surf.cal[mons >=7 & mons <= 9,]
#wtr.near.surf$year = as.POSIXlt(wtr.near.surf$DATETIME)$year+1900
num.dates = as.numeric(jas.surf.cal$DATETIME)/(365*60*60*24)
resids = jas.surf.cal$RESID

lm(resids ~ num.dates)

##Ok, with 54559 observations, this blows up
# and is choose(54559,2)*64/8/1e6 megabyes worth of pairs
#zyp.sen(resids ~ num.dates)
decimate.i = sample(1:length(resids), length(resids)/10)
resids.d = resids[decimate.i]
num.dates.d = num.dates[decimate.i]

zyp.sen(resids.d ~ num.dates.d)

################################################################################
## Resid trend vs depth
################################################################################


my.mat = interp(all.cal$YEAR, all.cal$DEPTH, all.cal$RESID, duplicate="median")
#tiff('kd.area.slope.heat.tiff', width=1800, height=1200, res=300, compression='lzw')
filled.contour(my.mat, color.palette=
							 	colorRampPalette(c("violet","blue","cyan", "green3", "yellow", "orange", "red"), 
							 									 bias = 1, space = "rgb"), ylab="log(kd)", xlab="log(area)", main='wtr.slope')





