#asdf
source('sens.confint.mod.R')

all.slopes = fread('all.slopes.csv')

## drop LTER lakes
lter = rbind(fread('../supporting files/lter.north.lakes.tsv'), fread('../supporting files/lter.south.lakes.tsv'))
all.slopes = all.slopes[(wbic %in% lter$WBIC), ]

#n.vals = fread('all.slopes.n.csv')
#all.slopes = merge(all.slopes, n.vals, by=c('wbic','week','depth'))

## nobs bin by area
all.slopes = all.slopes[order(area),]
all.slopes = all.slopes[!is.na(area),]
vec = all.slopes$area
nObs = 100000
datLabels <- ceiling(seq_along(vec)/nObs)[rank(vec, ties.method = "first")]
all.slopes[,bin.lab:=datLabels]

all.slopes = all.slopes[,rel.depth:=floor(10*depth/zmax)/10]
all.slopes = all.slopes[rel.depth <= 1,]

tiff('grl.figure.4.tiff', width=2400, height=2100, res=300, compression='lzw')
par(oma=c(0, 0.5, 0, 0), mar=c(5.1,4.1,4.1,3.1))
#par(mgp=c(1.5, 0.35, 0), tcl=-0.25, ps=10, family="Times") 
vals = ddply(all.slopes[bin.lab==1,], 'rel.depth', function(df)median(df$slopes))
confint = ddply(all.slopes[bin.lab==1,], 'rel.depth', function(df)sens.confint.mod.hirsch(df$slopes,df, pval=0.95))

plot(vals$rel.depth, vals$V1, ylim=c(-0.05, 0.1), type='o', lwd=1, 
		 ylab=expression(Median~Trend~(degree*C~yr^-1)),
		 xlab=expression(Relative~Depth~~~~~~~~~~~~~~~~~~''), #this is kinda a hack
		 xlim=c(0,1.3),bty='n', xaxt="n")
#abline(0, 0, lwd=2, lty=2)
lines(c(0,1), c(0,0), lwd=2, lty=2)
text(vals[11,]$rel.depth+0.02, vals[11,]$V1, expression(''<0.37~km^2), adj=c(0,0.2))
axis(1, at=c(0,1), labels=c("Surface", "Bottom"))


i=2
vals = ddply(all.slopes[bin.lab==i,], 'rel.depth', function(df)median(df$slopes))
confint = ddply(all.slopes[bin.lab==i,], 'rel.depth', function(df)sens.confint.mod.hirsch(df$slopes,df, pval=0.95))
lines(vals$rel.depth, vals$V1, ylim=c(-0.2, 0.2), type='o')
text(vals[11,]$rel.depth+0.02, vals[11,]$V1, expression(0.37-0.63~km^2), adj=c(0,0.1))

i=3
vals = ddply(all.slopes[bin.lab==i,], 'rel.depth', function(df)median(df$slopes))
confint = ddply(all.slopes[bin.lab==i,], 'rel.depth', function(df)sens.confint.mod.hirsch(df$slopes,df, pval=0.95))
lines(vals$rel.depth, vals$V1, ylim=c(-0.2, 0.2), type='o')
text(vals[11,]$rel.depth+0.02, vals[11,]$V1, expression(0.63-0.85~km^2), adj=c(0,0.2))

i=3
vals = ddply(all.slopes[bin.lab>i,], 'rel.depth', function(df)median(df$slopes))
confint = ddply(all.slopes[bin.lab==i,], 'rel.depth', function(df)sens.confint.mod.hirsch(df$slopes,df, pval=0.95))
lines(vals$rel.depth, vals$V1, ylim=c(-0.2, 0.2), type='o', lwd=2)
text(vals[11,]$rel.depth+0.02, vals[11,]$V1, expression(''>0.85~km^2), adj=c(0,0.2))

dev.off()




