## grl individual lake supplement figures
library(data.table)
library(plyr)
library(zyp)
source('sens.confint.mod.R')
all.slopes = fread('all.slopes.csv')
prefix = 'grl'

all.slopes = all.slopes[!is.na(area),]
all.slopes[,week:=week+1]

all.slopes[,week2:=(floor(week/2)*2)]
all.slopes[,week4:=(floor(week/4)*4)]
all.slopes = all.slopes[,rel.depth:=floor(10*depth/zmax)/10]
all.slopes = all.slopes[rel.depth <= 1,]


tiff('grl.figure.supp.tiff', width=1500, height=2000, res=300, compression='lzw')
par(mfrow=c(3,1), mar=c(1,5,0,0), oma=c(5,0,1,1))
##Trout Lake
boxplot(slopes~depth, all.slopes[wbic==2331600 & depth <= 32,], ylim=c(-0.3,0.3),
				xlim=c(0.8,33), xaxt='n')
abline(0,0, lty=2, lwd=2)
text(0.3, 0.27, "(a)")


boxplot(slopes~depth, all.slopes[wbic==1881900 & depth <= 18,], ylim=c(-0.3,0.3),
				 xlim=c(0.8,33), xaxt='n')
abline(0,0, lty=2, lwd=2)
text(0.3, 0.27, "(b)")
mtext(expression(Median~trend~(degree*C~yr^-1)), side=2, line=2)

##Trout Bog Lake
boxplot(slopes~depth, all.slopes[wbic==1842400 & depth <= 19,], ylim=c(-0.3,0.3),
				xlab='Depth (m)', xlim=c(0.8,33), xaxt='n')
abline(0,0, lty=2, lwd=2)
text(0.3, 0.27, "(c)")


axis(1, at=c(1:33), labels=as.character(c(0:32)))

mtext("Depth (m)", side=1, outer=TRUE, line=2, adj=0.6)

dev.off()
