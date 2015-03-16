
#all.slopes = fread('all.slopes.csv')
library(data.table)
library(plyr)
library(fields)
library(mda.lakes)
#source('../R/Libraries/GLM.functions.R')
source('luke.legend.R')
source('sens.confint.mod.R')
all.slopes = fread('all.slopes.csv')
wi = fread('../../../inst/supporting_files/WI_boundary_lat_lon.tsv')


wbic.slopes = ddply(all.slopes, 'wbic', function(df){
		return(data.frame(slope=median(df$slopes), n=nrow(df)))
	})

confint = ddply(all.slopes, 'wbic',
								function(df){sens.confint.mod.hirsch(df$slopes, data.table(df),pval=0.95)})

#if lower bound is > 0, then significant at 95%
confint$issig = (confint$V1 > 0 & confint$V2 > 0) | (confint$V1 < 0 & confint$V2 < 0)

wbic.slopes$lat = NA
wbic.slopes$lon = NA


for(i in 1:nrow(wbic.slopes)){
	lat.lon = getLatLon(as.character(wbic.slopes$wbic[i]))
	wbic.slopes$lat[i] = lat.lon[1]
	wbic.slopes$lon[i] = lat.lon[2]
}

wbic.slopes$cex = NA

wbic.slopes$cex[wbic.slopes$n < 3e3] = 0.5
wbic.slopes$cex[wbic.slopes$n >= 3e3] = 1.5

wbic.slopes = wbic.slopes[order(wbic.slopes$n, decreasing=TRUE),]

wbic.slopes = merge(wbic.slopes, confint) #merge is.sig into it

tiff('grl.figure.1.2.tiff', width=2400, height=3150, res=450, compression='lzw')

plot(wi$Lon, wi$Lat, type='l', lwd=2, bty='n', ylab='Lat', xlab='Lon', col='grey')


luke.colors = function(values){
	
	values[values < -0.1] = -0.1
	values[values > 0.1] = 0.1
	#l.levels = c(-0.2, -0.1, 0, 0.1, 0.2)
	
	my.col = color.scale(
		values, 
		zlim=c(-0.11,0.11),
		col=(two.colors(n=256, start="blue", end="red", middle="grey",
										alpha=1.0)),
		transparent.color="grey")
		
	return(my.col)
}

to.plot = wbic.slopes[!wbic.slopes$issig,]

points(to.plot$lon, to.plot$lat, 
			 col=luke.colors(to.plot$slope), bg=rgb(1,1,1,0),
			 pch=21, cex=to.plot$cex)#cex=log(wbic.slopes$n+10))

to.plot = wbic.slopes[wbic.slopes$issig,]

points(to.plot$lon, to.plot$lat, 
			 col=luke.colors(to.plot$slope), bg=luke.colors(to.plot$slope),
			 pch=21, cex=to.plot$cex)#cex=log(wbic.slopes$n+10))

l.levels = c(-0.1, -0.05, 0, 0.05, 0.1)

my.col = luke.colors(l.levels)

luke.legend("topright", fill = my.col, title=expression(Trend~degree*C~yr^-1),
			 legend = c("<= -0.1", "-0.05", "0", "0.05", ">= 0.1"),
			 horiz=FALSE,x.intersp=0.3, y.intersp=1, adj=c(-0.1,0.5), 
			 title.adj=c(0,0.0), bty='n', fill.x.cex=1.2, fill.y.cex=1.3)

dev.off()


