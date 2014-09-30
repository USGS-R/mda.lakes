

library(LakeMetabolizer)
library(data.table)
library(zyp)

Sys.setenv(TZ='GMT')

ap = load.all.data('airportmet', 'D:\\Dropbox\\Work\\Clean Buoy Data\\AirportMet')$data
#ap = data.table(ap)
#setkey(ap,datetime)
ap = data.table(ap)
ap[,dayonly:=as.POSIXct(trunc(datetime,units='days'))]

ap.daily = ap[,lapply(.SD, mean, na.rm=TRUE), by=dayonly]
ap.daily = data.frame(ap.daily)


reanal = read.table('D:\\WilmaDrivers\\07-30\\WBIC_2331600.csv', sep=',', header=TRUE)
reanal$time = as.POSIXct(reanal$time)
names(reanal) = paste("re", names(reanal), sep='_')

all.data = merge(reanal, data.table(ap.daily), by.x='re_time', by.y='dayonly')

all.data$rh[which(all.data$rh > 100)] = NA
all.data$lw[which(all.data$lw > 500)] = NA


first = 'sw'
second = 're_ShortWave'

plot(all.data[, first], all.data[, second])
abline(0,1, col='blue')
abline(lm(all.data[, second] ~ all.data[, first]), col='red', lwd=2)
lm(all.data[,second] ~ all.data[,first])


first = 'rh'
second = 're_RelHum'

plot(all.data[, first], all.data[, second])
abline(0,1, col='blue')
abline(lm(all.data[, second] ~ all.data[, first]), col='red', lwd=2)
lm(all.data[,second] ~ all.data[,first])

first = 'lw'
second = 're_LongWave'

plot(all.data[, first], all.data[, second])
abline(0,1, col='blue')
abline(lm(all.data[, second] ~ all.data[, first]), col='red', lwd=2)
lm(all.data[,second] ~ all.data[,first])

tmp = zyp.sen(re_LongWave ~ lw,all.data)
abline(0,tmp$coefficients[2])
