
#all.slopes = fread('all.slopes.csv')
library(data.table)
library(plyr)
library(fields)
source('../R/Libraries/GLM.functions.R')
source('luke.legend.R')
source('sens.confint.mod.R')
all.slopes = fread('all.slopes.csv')
wi = fread('../supporting files/WI_boundary_lat_lon.tsv')


wbic.slopes = ddply(all.slopes, 'wbic', function(df){
		return(data.frame(slope=median(df$slopes), n=nrow(df)))
	})

confint = ddply(all.slopes[wbic %in% wbic.slopes[wbic.slopes$n > 800,]$wbic], 'wbic',
								function(df){sens.confint.mod(df$slopes, pval=0.95)})


wbic.slopes$lat = NA
wbic.slopes$lon = NA


for(i in 1:nrow(wbic.slopes)){
	lat.lon = getLatLon(as.character(wbic.slopes$wbic[i]))
	wbic.slopes$lat[i] = lat.lon[1]
	wbic.slopes$lon[i] = lat.lon[2]
}

wbic.slopes$cex = NA

wbic.slopes$cex[wbic.slopes$n < 3e3] = 0.5
wbic.slopes$cex[wbic.slopes$n >= 3e3] = 2

wbic.slopes = wbic.slopes[order(wbic.slopes$n, decreasing=TRUE),]

tiff('grl.figure.1.1.tiff', width=1600, height=2100, res=300, compression='lzw')

plot(wi$Lon, wi$Lat, type='l', lwd=2, bty='n', ylab='Lat', xlab='Lon', col='grey')

points(wbic.slopes$lon, wbic.slopes$lat, 
			 col=color.scale(
			 		wbic.slopes$slope, 
			 		zlim=c(-0.5,0.5),
			 		col=(tim.colors(256,alpha=255)),
			 		transparent.color="grey"),
			 pch=16, cex=wbic.slopes$cex)#cex=log(wbic.slopes$n+10))

l.levels = c(-0.49,-0.25, 0, 0.25, 0.49, 0.6)
my.col = color.scale(
						l.levels, 
						zlim=c(-0.5,0.5),
						col=(tim.colors(256,alpha=255)),
						transparent.color="grey")

luke.legend("topright", fill = my.col, title=expression(Trend~degree*C~yr^-1),
			 legend = c("-0.5", "-0.25", "0", "0.25", "0.5", "NA"),
			 horiz=FALSE,x.intersp=0.3, y.intersp=1, adj=c(-0.1,0.5), 
			 title.adj=c(0,0.0), bty='n', fill.x.cex=1.2, fill.y.cex=1.3)

dev.off()



confint = ddply(all.slopes, 'week',
								function(df){sens.confint.mod(df$slopes, pval=0.95)})

