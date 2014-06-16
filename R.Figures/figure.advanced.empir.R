library(data.table)
source('../R/Libraries/GLM.functions.R')

all.slopes = fread('all.slopes.csv')
meta = data.frame(wbic=u.wbic, area=areas, kd=kds)

boxplot(slopes~week, all.slopes[depth>10,], ylim=c(-1,1))
abline(0,0,lwd=2)
by.month = all.slopes[,median(slopes), by=month]



all.slopes = data.frame(all.slopes)
slopes.by.lake = ddply(all.slopes, 'wbic', function(df){if(nrow(df)>20){median(df$slopes)}})
slopes.by.lake = merge(slopes.by.lake, meta)

plot(slopes.by.lake$area, slopes.by.lake$V1, xlim=c(0,8e7), ylim=c(-2,2))
abline(0,0)
x = seq(0, 8e7, by=100)
tmp = loess(V1~area, slopes.by.lake)
lines(x, predict(tmp, x), col='red')


#plot(all.slopes[area<9e7]$area, all.slopes[area<9e7]$slopes)
