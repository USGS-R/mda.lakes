# New fig 4
source('sens.confint.mod.R')
library(data.table)
library(plyr)

median.slopes = data.frame()
cutoff = 5e5
source('grl.resample.calcs.R')

for(l in 1:1000){
	
	sampled.slopes = data.table(grl.resample.calcs())
	
	sampled.slopes = sampled.slopes[!is.na(area),]
	
	sampled.slopes = sampled.slopes[wbic%in%sample(unique(sampled.slopes$wbic),100), ]
	
	ld = median(sampled.slopes[area > cutoff & rel.depth > 0.5,]$slopes, na.rm=TRUE)
	sd = median(sampled.slopes[area < cutoff & rel.depth > 0.5,]$slopes, na.rm=TRUE)
  ls = median(sampled.slopes[area > cutoff & rel.depth <= 0.5,]$slopes, na.rm=TRUE)
  ss = median(sampled.slopes[area < cutoff & rel.depth <= 0.5,]$slopes, na.rm=TRUE)
	al = median(sampled.slopes$slopes, na.rm=TRUE)
	
	median.slopes = rbind(median.slopes, 
				data.frame(large.deep=ld, small.deep=sd, large.shallow=ls, small.shallow=ss, all.lakes=al))
	cat(l,'\n')
}




tiff('new.figure.4.3.tiff', width=2000, height=2000, res=450, compression='lzw')
i=2
order = c(2.2, 2, 1.2, 1)
tmp = median.slopes[,i]
tmp = sort(tmp)
plot(rep(order[i], length(tmp)), tmp,  xlim=c(0.8,2.5), ylim=c(-0.05,0.1), 
		 pch=16, col=rgb(0.4,0.4,0.4,0.1),xaxt="n", xlab='', ylab=expression(Median~Trend~(degree*C~yr^-1)))
abline(0,0, lwd=2, lty=2, col=rgb(0,0,0,0.8))
i=4
tmp = median.slopes[,i]
points(rep(order[i], length(tmp)), tmp, pch=16, col=rgb(0.4,0.4,0.4,0.1))

for(i in c(1,3)){
	tmp = median.slopes[,i]
	points(rep(order[i], length(tmp)), tmp, pch=16, col=rgb(0.4,0.4,0.4,0.1))
}


median.slopes = data.frame(lapply(median.slopes, sort))

medians = unlist(lapply(median.slopes, median))
means = unlist(lapply(median.slopes, mean))
gr = rgb(0.5,0.5, 0.5)

order = c(2.2, 2, 1.2, 1)
col = c('white',gr,'white',gr)
	

top = unlist(median.slopes[975,1:4])
bottom = unlist(median.slopes[25,1:4])

top    = unlist(lapply(median.slopes, mean)) + 2*unlist(lapply(median.slopes, sd))
bottom = unlist(lapply(median.slopes, mean)) - 2*unlist(lapply(median.slopes, sd))

off = 0.03
for(i in 1:4){
	lines(c(order[i],order[i]), c(top[i], bottom[i]), lwd=2, col=rgb(0,0,0,1))
	lines(c(order[i]-off,order[i]+off), c(top[i], top[i]), lwd=2, col='black')
	lines(c(order[i]-off,order[i]+off), c(bottom[i], bottom[i]), lwd=2, col='black')
}
par()
axis(1, at=c(1.1,2.1), labels=c('Shallow\nWaters', 'Deep\nWaters'), mgp = c(0, 1.5, 0))
legend('topright', legend=c(expression(''<0.5~km^2), expression(''>0.5~km^2)), 
			 horiz=TRUE, pch=c(23,23),  bty='n', 
			 pt.bg=c(rgb(0.9,0.9,0.9,1),rgb(0.2,0.2,0.2,1)),
			 x.intersp=0.2,y.intersp=1.2, adj=c(0,0.25), inset=-0.01, 
			 pt.cex=1.5, lwd=2, lty='blank', seg.len=1)


points(order[c(2,4)], means[c(2,4)], pch=23, cex=1.5, 
			 col=c('black','black','black','black'),
			 xlab='',xaxt="n", bg=c(rgb(0.9,0.9,0.9,1)), lwd=2)

points(order[c(1,3)], means[c(1,3)], pch=23, cex=1.5, 
			 col=c('black','black','black','black'),
			 xlab='',xaxt="n", bg=c(rgb(0.2,0.2,0.2,1)), lwd=2)


prefix = 'downsamp'
dev.off()









## drop LTER lakes
#lter = rbind(fread('../supporting files/lter.north.lakes.tsv'), fread('../supporting files/lter.south.lakes.tsv'))
#all.slopes = all.slopes[(wbic %in% lter$WBIC), ]

#n.vals = fread('all.slopes.n.csv')
#all.slopes = merge(all.slopes, n.vals, by=c('wbic','week','depth'))

## nobs bin by area
all.slopes = all.slopes[order(area),]
all.slopes = all.slopes[!is.na(area),]
vec = all.slopes$area
nObs = 800*30
datLabels <- ceiling(seq_along(vec)/nObs)[rank(vec, ties.method = "first")]
all.slopes[,bin.lab:=datLabels]

all.slopes = all.slopes[,rel.depth:=floor(5*depth/zmax)/5]
all.slopes = all.slopes[rel.depth <= 1,]

all.slopes[,group:='']
all.slopes[area < 5e5 & rel.depth > 0.5, group:='small deep']
all.slopes[area < 5e5 & rel.depth < 0.5, group:='small shallow']
all.slopes[area > 5e5 & rel.depth < 0.5, group:='large shallow']
all.slopes[area > 5e5 & rel.depth > 0.5, group:='large deep']

boxplot(slopes~group, all.slopes, ylim=c(-0.4,0.4), at=c(1,2,4,5), 
				col=c('white','gray','white','gray'))



