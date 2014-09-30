#asdf
source('sens.confint.mod.R')
library(data.table)
library(plyr)

all.slopes = fread('downsampled.slopes.csv')
prefix = 'downsamp'

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

tiff(paste(prefix,'.figure.4.tiff',sep=''), width=2400, height=3000, res=300, compression='lzw')

par(oma=c(5.1, 0.5, 0, 0), mar=c(0,4.1,0,3.1), mfrow=c(2,1))
#par(mgp=c(1.5, 0.35, 0), tcl=-0.25, ps=10, family="Times") 
vals = ddply(all.slopes[area<5e5,], 'rel.depth', function(df)median(df$slopes))
confint = ddply(all.slopes[area<5e5,], 'rel.depth', function(df)sens.confint.mod.hirsch(df$slopes,df, pval=0.95))

plot(vals$rel.depth, vals$V1, ylim=c(-0.05, 0.1), type='o', lwd=1, 
		 ylab=expression(Median~Trend~(degree*C~yr^-1)),
		 xlab=expression(Relative~Depth~~~~~~~~~~~~~~~~~~''), #this is kinda a hack
		 xlim=c(0,1.3),bty='n', xaxt="n")
#abline(0, 0, lwd=2, lty=2)
lines(c(0,1), c(0,0), lwd=2, lty=2)
text(vals[6,]$rel.depth+0.02, vals[6,]$V1, expression(''<0.5~km^2), adj=c(0,0.2))
#axis(1, at=c(0,1), labels=c("Surface", "Bottom"))


# i=2
# vals = ddply(all.slopes[bin.lab==i,], 'rel.depth', function(df)median(df$slopes))
# confint = ddply(all.slopes[bin.lab==i,], 'rel.depth', function(df)sens.confint.mod.hirsch(df$slopes,df, pval=0.95))
# lines(vals$rel.depth, vals$V1, ylim=c(-0.2, 0.2), type='o')
# text(vals[11,]$rel.depth+0.02, vals[11,]$V1, expression(0.37-0.63~km^2), adj=c(0,0.1))
# 
# i=3
# vals = ddply(all.slopes[bin.lab==i,], 'rel.depth', function(df)median(df$slopes))
# confint = ddply(all.slopes[bin.lab==i,], 'rel.depth', function(df)sens.confint.mod.hirsch(df$slopes,df, pval=0.95))
# lines(vals$rel.depth, vals$V1, ylim=c(-0.2, 0.2), type='o')
# text(vals[11,]$rel.depth+0.02, vals[11,]$V1, expression(0.63-0.85~km^2), adj=c(0,0.2))

i=3
vals = ddply(all.slopes[area>5e5,], 'rel.depth', function(df)median(df$slopes))
confint = ddply(all.slopes[bin.lab==i,], 'rel.depth', function(df)sens.confint.mod(df$slopes, pval=0.95))
lines(vals$rel.depth, vals$V1, ylim=c(-0.2, 0.2), type='o', lwd=2)
text(vals[6,]$rel.depth+0.02, vals[6,]$V1, expression(''>0.5~km^2), adj=c(0,0.2))
text(0,0.095,'(a)')


all.slopes = fread('downsampled.slopes.density.csv')

## drop LTER lakes
#lter = rbind(fread('../supporting files/lter.north.lakes.tsv'), fread('../supporting files/lter.south.lakes.tsv'))
#all.slopes = all.slopes[(wbic %in% lter$WBIC), ]

#n.vals = fread('all.slopes.n.csv')
#all.slopes = merge(all.slopes, n.vals, by=c('wbic','week','depth'))

## nobs bin by area
all.slopes = all.slopes[order(area),]
all.slopes = all.slopes[!is.na(area),]
vec = all.slopes$area
nObs = 100000
datLabels <- ceiling(seq_along(vec)/nObs)[rank(vec, ties.method = "first")]
all.slopes[,bin.lab:=datLabels]

all.slopes = all.slopes[,rel.depth:=floor(5*depth/zmax)/5]
all.slopes = all.slopes[rel.depth <= 1,]


#par(oma=c(0, 0.5, 0, 0), mar=c(5.1,4.1,4.1,3.1))
#par(mgp=c(1.5, 0.35, 0), tcl=-0.25, ps=10, family="Times") 
vals = ddply(all.slopes[area<5e5,], 'rel.depth', function(df)median(df$slopes))
confint = ddply(all.slopes[area<5e5,], 'rel.depth', function(df)sens.confint.mod.hirsch(df$slopes,df, pval=0.95))

plot(vals$rel.depth, vals$V1, ylim=c(-0.015, 0.001), type='o', lwd=1, 
		 ylab=expression(Median~Trend~(Kg~L^-1~yr^-1)),
		 xlab=expression(Relative~Depth~~~~~~~~~~~~~~~~~~''), #this is kinda a hack
		 xlim=c(0,1.3),bty='n', xaxt="n")
#abline(0, 0, lwd=2, lty=2)
lines(c(0,1), c(0,0), lwd=2, lty=2)
text(vals[11,]$rel.depth+0.02, vals[11,]$V1, expression(''<0.5~km^2), adj=c(0,0.2))
axis(1, at=c(0,1), labels=c("Surface", "Bottom"))
text(0,-0.0005,'(b)')

# i=2
# vals = ddply(all.slopes[bin.lab==i,], 'rel.depth', function(df)median(df$slopes))
# confint = ddply(all.slopes[bin.lab==i,], 'rel.depth', function(df)sens.confint.mod.hirsch(df$slopes,df, pval=0.95))
# lines(vals$rel.depth, vals$V1, ylim=c(-0.2, 0.2), type='o')
# text(vals[11,]$rel.depth+0.02, vals[11,]$V1, expression(0.37-0.63~km^2), adj=c(0,0.1))
# 
# i=3
# vals = ddply(all.slopes[bin.lab==i,], 'rel.depth', function(df)median(df$slopes))
# confint = ddply(all.slopes[bin.lab==i,], 'rel.depth', function(df)sens.confint.mod.hirsch(df$slopes,df, pval=0.95))
# lines(vals$rel.depth, vals$V1, ylim=c(-0.2, 0.2), type='o')
# text(vals[11,]$rel.depth+0.02, vals[11,]$V1, expression(0.63-0.85~km^2), adj=c(0,0.2))

i=3
vals = ddply(all.slopes[area>5e5,], 'rel.depth', function(df)median(df$slopes))
confint = ddply(all.slopes[bin.lab==i,], 'rel.depth', function(df)sens.confint.mod(df$slopes, pval=0.95))
lines(vals$rel.depth, vals$V1, ylim=c(-0.2, 0.2), type='o', lwd=2)
text(vals[11,]$rel.depth+0.02, vals[11,]$V1, expression(''>0.5~km^2), adj=c(0,0.2))

dev.off()



