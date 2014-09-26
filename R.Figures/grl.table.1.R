
library(data.table)
library(plyr)
library(plotrix)
source('sens.confint.mod.R')
source('../R/Libraries/GLM.functions.R')

all.slopes = fread('all.slopes.csv')
prefix = 'grl'

#n.vals = fread('all.slopes.n.csv')
#all.slopes = merge(all.slopes, n.vals, by=c('wbic','week','depth'))

##equal bin ranked sizes
all.slopes = all.slopes[order(area),]
all.slopes = all.slopes[!is.na(area),]
vec = all.slopes$area
nObs = 100000
datLabels <- ceiling(seq_along(vec)/nObs)[rank(vec, ties.method = "first")]
all.slopes[,bin.lab:=datLabels]


u.wbic = unique(all.slopes$wbic)
areas = rep(NA, length(u.wbic))
lats = rep(NA, length(u.wbic))
lons = rep(NA, length(u.wbic))
elevs = rep(NA, length(u.wbic))

for(i in 1:length(areas)){
	areas[i] = getArea(as.character(u.wbic[i]))
	lats[i] = getLatLon(as.character(u.wbic[i]))[1]
	lons[i] = getLatLon(as.character(u.wbic[i]))[2]
	elevs[i]= getElevation(as.character(u.wbic[i]))
}

all.lakes = unique(all.slopes[,list(wbic,area, kd, zmax)])

cat("Number of lakes", length(unique(all.lakes$wbic)))
cat("IQL of lake sizes", IQR(areas)/1e6)
cat("Median of lake sizes", median(areas)/1e6)

range(all.lakes$zmax)
median(all.lakes$zmax)

range(all.lakes$kd, na.rm=T)
median(all.lakes$kd, na.rm=T)

median(lats)
range(lats)
median(lons)
range(lons)

median(elevs, na.rm=T)
range(elevs, na.rm=T)



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

wtr = wtr[year > 1979]
tmp = ddply(wtr, 'wbic', function(df)range(df$year))
tmp$n.year=tmp$V2 - tmp$V1

tmp = tmp[tmp$n.year > 1, ]

duration = tmp$n.year
breaks = seq(2, 33, by=1) 
duration.cut = cut(duration, breaks, right=FALSE)
duration.freq = table(duration.cut)

cumfreq0 = cumsum(duration.freq)
breaks = breaks[2:length(breaks)]

plot(breaks, rev(cumfreq0), ylim=c(0,900),
		    main="", 
		    xlab="Record duration (years)",
		    ylab="Number of lakes with record duration of x years or greater")
lines(breaks, rev(cumfreq0)) 

