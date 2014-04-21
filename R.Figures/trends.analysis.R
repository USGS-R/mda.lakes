# Trends analysis

library(plyr)
library(emdbook)
source('../R/Libraries/GLM.functions.R')

data = read.table('../Output/omg.huge.output.tsv', sep='\t', header=TRUE)

lake.meta = data.frame(WBIC=unique(data$lakeid), max.depth=NA, area=NA, kd=NA)

for(i in 1:nrow(lake.meta)){
  lake.meta$max.depth[i] = getZmax(as.character(lake.meta$WBIC[i]))
  lake.meta$area[i] = getArea(as.character(lake.meta$WBIC[i]))
  if(is.na(lake.meta$max.depth[i])){
    lake.meta$max.depth[i] = max(getBathy(as.character(lake.meta$WBIC[i]))$depth)
  }
  lake.meta$kd[i] = getClarity(as.character(lake.meta$WBIC[i]))
}


air.temp = read.table('../Output/airtemp.metrics.csv', sep=',', header=TRUE)

hyp.temps = read.table('../Output/KdScenarios/stable.metrics.out.tsv', sep='\t', header=TRUE)

all.data = merge(air.temp, data, by.x=c("WBIC", "Year"), by.y=c("lakeid", "year"))

all.data = merge(all.data, hyp.temps, by.x=c("WBIC", "Year"), by.y=c("lakeid", "year"))


jas.air.slopes = ddply(all.data, c('WBIC'), function(df) lm(df$JAS.Mean ~ df$Year)$coeff[2])

jas.surf.slopes = ddply(all.data, c('WBIC'), function(df) lm(df$mean_surf_JAS ~ df$Year)$coeff[2])
names(jas.surf.slopes) = c('WBIC','surf.slope')
names(jas.air.slopes) = c('WBIC','air.slope')


all.slopes= join(jas.surf.slopes, jas.air.slopes)
all.slopes = join(all.slopes, lake.meta)


hist(jas.air.slopes$`df$Year`, xlim=c(-0.07, 0.07), breaks=200)
hist(jas.surf.slopes$`df$Year`, xlim=c(-0.07, 0.07), breaks=200)

#Compare the slopes
plot(all.slopes$air.slope, all.slopes$surf.slope, ylim=c(-0.07, 0.07))
cat('Spearman rank test')
cor.test(all.slopes$air.slope, all.slopes$surf.slope, method='spearman')
cat('Linear test, terrible R^2')
summary(lm(all.slopes$air.slope ~ all.slopes$surf.slope))


surf.slopes.meta = merge(jas.surf.slopes, lake.meta)
plot(surf.slopes.meta$kd, surf.slopes.meta$surf.slope, ylim=c(-0.05,0.05), xlim=c(0,4))

ma = loess(surf.slope ~ kd, surf.slopes.meta)
x = seq(0, 4, by=0.1)
lines(x, predict(ma, x), col='red', lwd=2)

## Surface plot
clip.ssm = surf.slopes.meta[surf.slopes.meta$surf.slope > -0.01 & surf.slopes.meta$surf.slope < 0.01,]

colors = rainbow(nrow(surf.slopes.meta),alpha=0.4)[order(surf.slopes.meta$surf.slope,decreasing=TRUE)]

tiff('kd.area.slope.tiff', width=3600, height=3600, res=300, compression='lzw')
plot(log(surf.slopes.meta$area), log(surf.slopes.meta$kd), col=colors, pch=16)
dev.off()

my.mat = interp(log(clip.ssm$area), log(clip.ssm$kd), clip.ssm$surf.slope)
tiff('kd.area.slope.heat.tiff', width=1800, height=1200, res=300, compression='lzw')
filled.contour(my.mat, color.palette=
                 colorRampPalette(c("violet","blue","cyan", "green3", "yellow", "orange", "red"), 
                 bias = 1, space = "rgb"), ylab="log(kd)", xlab="log(area)", title='wtr.slope')
dev.off()


################################################################################
## Interesting Hypo results
################################################################################

hist(hyp.temps$mean_hypo_temp, breaks=100)

hyp.trend.func = function(df){
  
  if(any(is.na(df$mean_hypo_temp))){
    return(NA)
  }
  return(lm(df$mean_hypo_temp ~ df$year)$coeff[2])
}

hyp.trends = ddply(hyp.temps, c('lakeid'), hyp.trend.func)
names(hyp.trends) = c('WBIC','hyp.slope')
hyp.trends = hyp.trends[!is.na(hyp.trends$hyp.slope),]
hyp.trends = hyp.trends[hyp.trends$hyp.slope < 0.3 & hyp.trends$hyp.slope > -0.15, ]
hist(hyp.trends$hyp.slope, breaks=100)

################################################################################
## Slope Histograms
################################################################################

tiff('slope.dists.tiff', res=300, width=1200, height=1200, compression='lzw')
air.surf.slopes = merge(jas.air.slopes, jas.surf.slopes)

vals = air.surf.slopes[,-1]
N=2
limX <- range(vals)

dens <- apply(vals, 2, function(x)density(x, from=limX[1], to=limX[2])[c("x","y")])

dX <- lapply(dens, function(z)z$x)
dY <- lapply(dens, function(z)z$y)
limY <- range(dY)

cLine <- rainbow(n=N)
cFill <- rgb(t(col2rgb(cLine, alpha=TRUE)), alpha=35, maxColorValue=255)

# Need to be sure that the smallest and largest densities are 0 so that the bottom border of polygons are at 0 line
xF <- 0.01 * diff(limX) # a "factor" by which to extend the range of X
xA <- limX + c(-1,1)*xF # "add" this "adjustment" to the start and end of the dX

par(mar=c(1.5, 1.5, 0.5, 0.5), ps=10, cex=1, mgp=c(1.5, 0.1, 0), tcl=0.25, las=1)
plot(c(xA[1],dX[[1]],xA[2]), c(0,dY[[1]],0), type="l", col=cLine[1], xlab="", ylab="", xlim=limX, ylim=limY, lwd=2)
polygon(c(xA[1],dX[[1]],xA[2]), c(0,dY[[1]],0), col=cFill[1], border=NA)
for(i in 2:N){
  polygon(c(xA[1],dX[[1]],xA[2]), c(0,dY[[i]],0), col=cFill[i], border=cLine[i], lwd=2)
}

legend(-0.1, 80, c("Air Slopes", "Wtr Slopes"), fill=cFill)
dev.off()


################################################################################
### How about dur.strat
################################################################################
dur.strat.slopes = ddply(all.data, c('WBIC'), function(df) lm(df$durStrat.x ~ df$Year)$coeff[2])


