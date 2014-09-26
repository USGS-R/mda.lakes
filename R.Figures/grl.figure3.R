###

library(plotrix)
library(data.table)
source('sens.confint.mod.R')

bootstrap.estim = matrix(nrow=1000, ncol=5)


bins=c(0, 5e5, 1.3e6, 3e6, 6.4e6, 600e6)

for(l in 1:1000){
	source('grl.resample.calcs.R')
	all.slopes = fread('downsampled.slopes.csv')
	
	all.slopes = all.slopes[!is.na(area),]
	
	all.slopes = all.slopes[wbic%in%sample(unique(all.slopes$wbic),100), ]
	
	#tmp = all.slopes[area > bot & area < top,]
	#median(tmp$slopes)


	prefix = 'downsamp'
	
	##equal bin ranked sizes
	all.slopes = all.slopes[order(area),]
	all.slopes = all.slopes[!is.na(area),]
	vec = all.slopes$area
	nObs = 800*30
	datLabels <- ceiling(seq_along(vec)/nObs)[rank(vec, ties.method = "first")]
	all.slopes[,bin.lab:=datLabels]
	
	for(k in 1:5){
		bot= bins[k]
		top = bins[k+1]
		bootstrap.estim[l,k] = median(all.slopes[area > bot & area < top,]$slopes)
	}
	
	
	#slopes = ddply(all.slopes, 'bin.lab', function(df){c(median(df$slopes), min(df$area), max(df$area), mean(range(df$area)))})
	#confint = ddply(all.slopes, 'bin.lab',  
  #								function(df)sens.confint.mod.hirsch(df$slopes, df, pval=0.95))
	#confint = ddply(all.slopes, 'bin.lab', 
	#								function(df)sens.confint.mod(df$slopes, pval=0.95))
	
	#bootstrap.estim[l,] = slopes$V1
	cat(l, '\n')
}


tmp = data.frame(bootstrap.estim)
tmp = data.frame(lapply(tmp, sort))
medians = data.frame(lapply(tmp,median))


tiff('new.figure.3.tiff', width=2200, height=2200, res=450, compression='lzw')

plot(1:5, unlist(medians), ylim=c(-0.01,0.12), xlim=c(0.5,5.5),
		 xaxt='n', xlab=expression(Bin~Sizes~(km^2)), ylab=expression(Median~Trend~(degree*C~yr^-1)))

abline(0,0, lwd=2, lty=2, col=rgb(0,0,0,0.8))

for(i in 1:5){
	points(rep(i,nrow(tmp)), tmp[,i], pch=16, col=rgb(0.4,0.4,0.4,0.1))
}

top = unlist(tmp[950,])
bottom = unlist(tmp[50,])

off=0.075
for(i in 1:5){
	lines(c(i,i), c(top[i], bottom[i]), col='black', lwd=2)
	
	lines(c(i+off,i-off), c(top[i], top[i]), col='black', lwd=2)
	lines(c(i+off,i-off), c(bottom[i], bottom[i]), col='black', lwd=2)
}

#bins=c(0, 5e5, 1.3e6, 3e6, 6.4e6, 600e6)
points(1:5, unlist(medians), pch=23, cex=1.5, bg=rgb(0.4,0.4,0.4,1), lwd=2)
axis(1, at=1:5, labels=c("<0.5", "0.5-1.3", "1.3-3", "3-6.4", ">6.4"))

dev.off()




tiff(paste(prefix,'.figure.3.tiff', sep=''), width=2000, height=1500, res=300, compression='lzw')
par(oma=c(0,0.5,0,0))

plot(slopes$bin.lab, slopes$V1, ylim=c(-0.02, 0.2), xlim=c(0,6),
		 frame.plot=F, lwd=2, type='o',
		 #log='x',xaxt='n',
		 ylab=expression(Median~Trend~(degree*C~yr^-1)),
		 xlab=expression(Lake~Bin))
#axis(1, at=c(1,10), labels=c("", ""), col.ticks=FALSE )
#axis(1, at=seq(2,10,by=2), col.ticks=TRUE )
#lines(c(1,10),c(0,0), lwd=2, lty=2)
#abline(0, 0, lwd=2, lty=2)
lines(c(0,6), c(0,0), lwd=2, lty=2)
#axis(1, at=slopes$bin.lab, labels=as.character(round(slopes$V4/1e5)/10), lwd.ticks=1)


down = -0.06
up = 0.06
segments(slopes$bin.lab, confint$V1, slopes$bin.lab , confint$V2)
segments(slopes$bin.lab+down, confint$V1, slopes$bin.lab+up, confint$V1)
segments(slopes$bin.lab+down, confint$V2, slopes$bin.lab+up, confint$V2)

sizes = paste(floor(slopes$V2/1e5)/10, '-' , floor(slopes$V3/1e5)/10, sep='')
text(slopes$bin.lab, confint$V1, 
		 sizes, adj=c(0.5,-1))

#boxed.labels(slopes$bin.lab[10], 0.1, 
		 #sizes[10], adj=c(0.5,0), bg='white', border=NA)


#xval = 7e4
#lines(xval, median(all.slopes$slopes), type='o', lwd=2)
#axis(1, at=c(5e4,9e4), labels=c('',''), lwd.ticks=0)
#axis(1, at=xval, labels="All Lakes")

#confint = sens.confint.mod.hirsch(all.slopes$slopes, all.slopes)
#segments(xval, confint[1], xval, confint[2])
#segments(xval*0.94, confint[1], xval*1.06, confint[1])
#segments(xval*0.94, confint[2], xval*1.06, confint[2])
dev.off()





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
