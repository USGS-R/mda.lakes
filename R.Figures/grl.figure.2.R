## grl figure 2
library(data.table)
library(plyr)
library(zyp)
source('sens.confint.mod.R')
all.slopes = fread('all.slopes.csv')
all.slopes = all.slopes[!is.na(area),]
all.slopes[,week:=week+1]

all.slopes[,week2:=(floor(week/2)*2)]
all.slopes[,week4:=(floor(week/4)*4)]
all.slopes = all.slopes[,rel.depth:=floor(10*depth/zmax)/10]
all.slopes = all.slopes[rel.depth <= 1,]

tiff('grl.figure.2.tiff', width=2000, height=2000, res=300, compression='lzw')
par(mfrow=c(2,1), mar=c(1,5,0,0), oma=c(5,0,1,0))

slopes = ddply(all.slopes,'week', function(df)median(df$slopes))
confint.zyp(zyp.sen(V1~week, slopes))

confint = ddply(all.slopes, 'week',
											function(df){sens.confint.mod(df$slopes, pval=0.95)})
#confint = ddply(all.slopes, 'week',
#								function(df){sens.confint.mod(df$slopes, pval=0.95)})



plot(slopes$week, slopes$V1, ylim=c(-0.1, 0.4),
		 frame.plot=F, xlim=c(0,52),
		 xaxt='n',
		 ylab=expression(Median~Trend~(degree*C~yr^-1)),
		 xlab=expression(Week~of~year))
text(1, 0.35, "(a)")
#axis(1)
#axis(1, at=seq(2,10,by=2), col.ticks=TRUE )
#lines(c(1,10),c(0,0), lwd=2, lty=2)
#abline(0, 0, lwd=2, lty=2)
lines(c(1,51), c(0,0), lty=2, lwd=2)
#axis(1, at=slopes$V4, labels=as.character(round(slopes$V4/1e5)/10), lwd.ticks=1)


down = 0.06
up = -0.06
segments(confint$week, confint$V1, confint$week, confint$V2)
segments(confint$week+down, confint$V1, confint$week+up, confint$V1)
segments(confint$week+down, confint$V2, confint$week+up, confint$V2)



hist(all.slopes$week, main='', xlab=expression(Week~of~year), 
		 ylab="Observation Density", yaxt="n", breaks=52, xlim=c(0,52))
axis(2, at=c(0, 25000, 50000, 75000, 1e5), labels=c("0", "25,000", "50,000", "", ""))
text(1, 55000, "(b)")
dev.off()
