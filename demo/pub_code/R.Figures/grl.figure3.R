###

library(plotrix)
library(data.table)
library(plyr)
source('sens.confint.mod.R')

bootstrap.estim = matrix(nrow=1000, ncol=5)


bins=c(0, 5e5, 1.3e6, 3e6, 6.4e6, 600e6)


## Just a try
all.med.area = c()
all.med.slope = c()
all.index = c()

for(l in 1:1000){
	source('grl.resample.calcs.R')
	all.slopes = fread('downsampled.slopes.csv')
	
	meandt = ddply(all.slopes,'wbic',function(df)mean(df$dt))
	#meandt = meandt#[meandt$V1 > 5,]
	all.slopes = all.slopes[wbic%in%sample(unique(all.slopes$wbic),100), ]
	nrow(meandt)
	
	#all.slopes = all.slopes[wbic%in%meandt$wbic, ]
	
	wbic.size = unique(all.slopes[,list(wbic,area)])
	wbic.size = wbic.size[order(area),]
	
	n = nrow(wbic.size)-30
	med.area = rep(NA,n)
	med.slope = rep(NA,n)
	
	for(i in 1:n){
		
		bot = wbic.size[i:(i+29),]
		
		med.slope[i] = median(all.slopes[wbic%in%bot$wbic,]$slopes)
		med.area[i] = median(bot$area)
		
	}
	all.med.area = c(all.med.area, med.area)
	all.med.slope = c(all.med.slope, med.slope)
	all.index = c(all.index, med.slope = rep(l,n))
	cat(l,'\n')
}

tiff('new.figure.3.2.tiff', width=2000, height=1500, res=300, compression='lzw')

plot(all.med.area,all.med.slope, log='x', xaxt='n', pch=20, bg=rgb(0.5,0.5,0.5, 0.025), col=rgb(0.5,0.5,0.5, 0.05),
		 ylab=expression(Median~Trend~(degree*C~yr^-1)), xlab=expression(Median~Lake~Area~(km^2)), cex=0.5)

tmp = loess(all.med.slope~all.med.area)
lines(sort(all.med.area), predict(tmp, sort(all.med.area)), lwd=2, col='black')
axis(1, at=c(5e5, 1e6, 2e6, 5e6, 1e7), 
		 labels=c("0.5", "1", "2", "5", "10"))
		 #labels=c(expression(''*5*x*10^5),expression(''*1*x*10^6),expression(''*2*x*10^6),expression(''*5*x*10^6), expression(''*1*x*10^7)))

dev.off()
