###


source('sens.confint.mod.R')
all.slopes = fread('all.slopes.csv')
n.vals = fread('all.slopes.n.csv')
all.slopes = merge(all.slopes, n.vals, by=c('wbic','week','depth'))

##equal bin ranked sizes
all.slopes = all.slopes[order(area),]
all.slopes = all.slopes[!is.na(area),]
vec = all.slopes$area
nObs = 100000
datLabels <- ceiling(seq_along(vec)/nObs)[rank(vec, ties.method = "first")]
all.slopes[,bin.lab:=datLabels]


#par(mfcol=c(1,2))
#par(oma=c(1,0.5,0,0))
#layout(matrix(c(1,1,2,2,2,2), 2, 3, byrow = FALSE))


tiff('grl.figure.2.tiff', width=3200, height=1150, res=300, compression='lzw')

par(oma=c(0,0.5,0,0))
slopes = ddply(all.slopes, 'bin.lab', function(df){c(median(df$slopes), min(df$area), max(df$area), mean(range(df$area)))})
confint = ddply(all.slopes, 'bin.lab', 
								function(df)sens.confint.mod.hirsch(df$slopes, df, pval=0.95))

plot(slopes$V4, slopes$V1, ylim=c(-0.04, 0.1), xlim=c(5e4,5e8),
		 frame.plot=F, lwd=2, type='o',
		 log='x', xaxt='n',
		 ylab=expression(Median~Trend~(degree*C~yr^-1)),
		 xlab=expression(Lake~Bin~Midpoint~(km^2)))
#axis(1, at=c(1,10), labels=c("", ""), col.ticks=FALSE )
#axis(1, at=seq(2,10,by=2), col.ticks=TRUE )
#lines(c(1,10),c(0,0), lwd=2, lty=2)
abline(0, 0, lwd=2, lty=2)
axis(1, at=slopes$V4, labels=as.character(round(slopes$V4/1e5)/10), lwd.ticks=1)


down = 0.97
up = 1.03
segments(slopes$V4, confint$V1, slopes$V4 , confint$V2)
segments(slopes$V4*down, confint$V1, slopes$V4*up, confint$V1)
segments(slopes$V4*down, confint$V2, slopes$V4*up, confint$V2)

xval = 7e4
lines(xval, median(all.slopes$slopes), type='o', lwd=2)
axis(1, at=c(5e4,9e4), labels=c('',''), lwd.ticks=0)
axis(1, at=xval, labels="All Lakes")

confint = sens.confint.mod.hirsch(all.slopes$slopes, all.slopes)
segments(xval, confint[1], xval, confint[2])
segments(xval*0.94, confint[1], xval*1.06, confint[1])
segments(xval*0.94, confint[2], xval*1.06, confint[2])
dev.off()

