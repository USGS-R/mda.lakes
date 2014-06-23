library(data.table)

all.slopes = fread('all.slopes.csv')


boxplot(slopes~week, all.slopes[depth>2,], ylim=c(-1,1))
abline(0,0,lwd=2)
by.month = all.slopes[,median(slopes), by=month]

plot(all.slopes$area, all.slopes$slopes)


all.slopes = data.frame(all.slopes)
slopes.by.lake = ddply(all.slopes, 'wbic', function(df){if(nrow(df)>20){median(df$slopes)}})
slopes.by.lake = merge(slopes.by.lake, meta)

plot(slopes.by.lake$area, slopes.by.lake$V1)
abline(0,0)
x = seq(0, 8e7, by=100)
tmp = loess(V1~area, slopes.by.lake)
lines(x, predict(tmp, x), col='red')


plot(all.slopes$dt, all.slopes$slopes)
abline(0,0,lwd=2)
x=0:40
tmp = loess(slopes~dt, all.slopes)
lines(x, predict(tmp, x), col='red')

boxplot(slopes~dt, all.slopes, ylim=c(-1,1))
abline(0,0,lwd=2)


#plot(all.slopes[area<9e7]$area, all.slopes[area<9e7]$slopes)
all.slopes[,size.class:=floor(log10(area)*2)]
par(mfrow=c(2,1))
boxplot(slopes~size.class, all.slopes, ylim=c(-1,1))
abline(0,0,lwd=2)
hist(all.slopes$size.class)


## nobs bin by area
all.slopes = all.slopes[order(area),]
all.slopes = all.slopes[!is.na(area),]
vec = all.slopes$area
nObs = 100000
datLabels <- ceiling(seq_along(vec)/nObs)[rank(vec, ties.method = "first")]
all.slopes[,bin.lab:=datLabels]

par(mfrow=c(2,1))
boxplot(slopes~bin.lab, all.slopes, ylim=c(-0.4,0.4))
abline(0,0,lwd=2)
hist(all.slopes$bin.lab)


## start/stop bins
par(mfrow=c(2,1))
boxplot(slopes~start, all.slopes, ylim=c(-0.4,0.4))
abline(0,0,lwd=2)
hist(all.slopes$start)

par(mfrow=c(2,1))
boxplot(slopes~end, all.slopes, ylim=c(-0.4,0.4))
abline(0,0,lwd=2)
hist(all.slopes$end)



par(mfrow=c(2,1))
boxplot(slopes~depth, all.slopes, ylim=c(-0.4,0.4))
abline(0,0,lwd=2)
hist(all.slopes$depth)

par(mfrow=c(2,1))
boxplot(slopes~depth, all.slopes, ylim=c(-0.4,0.4))
abline(0,0,lwd=2)
hist(all.slopes$depth)

## Kd nobs bin
all.slopes = all.slopes[order(kd),]
all.slopes = all.slopes[!is.na(kd),]
vec = all.slopes$kd
nObs = 100000
datLabels <- ceiling(seq_along(vec)/nObs)[rank(vec, ties.method = "first")]
all.slopes[,kd.bin:=datLabels]
par(mfrow=c(2,1))
boxplot(slopes~kd.bin, all.slopes, ylim=c(-0.4,0.4))
abline(0,0,lwd=2)
hist(all.slopes$kd.bin)


## By full seasons
par(mfrow=c(2,1))
all.slopes[,season:=ceiling(week/13)]
boxplot(slopes~season, all.slopes, ylim=c(-0.4,0.4))
abline(0,0,lwd=2)
hist(all.slopes$season)

## 10 years or longer
long.slopes = all.slopes[dt>=10]
long.slopes = long.slopes[order(area),]
long.slopes = long.slopes[!is.na(area),]
vec = long.slopes$area
nObs = 50000
datLabels <- ceiling(seq_along(vec)/nObs)[rank(vec, ties.method = "first")]
long.slopes[,bin.lab:=datLabels]

par(mfrow=c(2,1))
boxplot(slopes~bin.lab, long.slopes, ylim=c(-0.4,0.4))
abline(0,0,lwd=2)
hist(long.slopes$bin.lab)

## Long slopes bey week
boxplot(slopes~week, long.slopes[depth>2,], ylim=c(-0.4,0.4))
abline(0,0,lwd=2)
abline(0.05,0,col='red')



