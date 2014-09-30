
library(plyr)
library(emdbook)
source('../R/Libraries/GLM.functions.R')

data = read.table('../Output/omg.huge.output.tsv', sep='\t', header=TRUE)
wi = read.table('../supporting files/WI_boundary_lat_lon.tsv', sep='\t', header=TRUE)
driver.data = read.table('../Output/driver.JAS.means.tsv', sep='\t', header=TRUE)


lake.meta = data.frame(WBIC=unique(data$lakeid), max.depth=NA, area=NA, kd=NA, lat=NA, lon=NA)

for(i in 1:nrow(lake.meta)){
  lake.meta$max.depth[i] = getZmax(as.character(lake.meta$WBIC[i]))
  lake.meta$area[i] = getArea(as.character(lake.meta$WBIC[i]))
  if(is.na(lake.meta$max.depth[i])){
    lake.meta$max.depth[i] = max(getBathy(as.character(lake.meta$WBIC[i]))$depth)
  }
  lake.meta$kd[i] = getClarity(as.character(lake.meta$WBIC[i]))
  ll = getLatLon(as.character(lake.meta$WBIC[i]))
  lake.meta$lat[i] = ll[1]
  lake.meta$lon[i] = ll[2]
}


air.temp = read.table('../Output/airtemp.metrics.csv', sep=',', header=TRUE)

hyp.temps = read.table('../Output/KdScenarios/stable.metrics.out.tsv', sep='\t', header=TRUE)

all.data = merge(air.temp, data, by.x=c("WBIC", "Year"), by.y=c("lakeid", "year"))

all.data = merge(all.data, hyp.temps, by.x=c("WBIC", "Year"), by.y=c("lakeid", "year"))




jas.air.slopes = ddply(all.data, c('WBIC'), function(df) lm(df$JAS.Mean ~ df$Year)$coeff[2])

jas.surf.slopes = ddply(all.data, c('WBIC'), function(df) lm(df$mean_surf_JAS ~ df$Year)$coeff[2])
names(jas.surf.slopes) = c('WBIC','surf.slope')
names(jas.air.slopes) = c('WBIC','air.slope')


sw.slopes = ddply(driver.data, c('WBIC'), function(df) lm(df$ShortWave ~ df$year)$coeff[2])
names(sw.slopes) = c('WBIC','sw.slope')
lw.slopes = ddply(driver.data, c('WBIC'), function(df) lm(df$LongWave ~ df$year)$coeff[2])
names(lw.slopes) = c('WBIC','lw.slope')
ws.slopes = ddply(driver.data, c('WBIC'), function(df) lm(df$WindSpeed ~ df$year)$coeff[2])
names(ws.slopes) = c('WBIC','ws.slopes')
at.slopes = ddply(driver.data, c('WBIC'), function(df) lm(df$AirTemp ~ df$year)$coeff[2])
names(at.slopes) = c('WBIC','at.slope')

driver.slopes = merge(merge(sw.slopes,lw.slopes), merge(ws.slopes, at.slopes))

all.slopes= join(jas.surf.slopes, jas.air.slopes)
all.slopes = join(all.slopes, lake.meta)
all.slopes = join(all.slopes, driver.slopes)

colors = rainbow(nrow(all.slopes),alpha=0.4)[order(all.slopes$surf.slope, decreasing=TRUE)]
plot(all.slopes$lon, all.slopes$lat, col=colors, pch=16)
#wi = wi[!is.na(wi$Lon),]
lines(wi$Lon, wi$Lat)


tiff('wtr.trend.tiff', width=1800, height=1800, res=300, compression='lzw')


my.mat = interp(all.slopes$lon, all.slopes$lat, all.slopes$surf.slope)

filled.contour(my.mat, color.palette=
                 colorRampPalette(c("violet","blue","cyan", "green3", "yellow", "orange", "red"), 
                 bias = 1, space = "rgb"), ylab="Lat", xlab="Lon", main='Wtr Trend',
               plot.axes =  lines(wi$Lon, wi$Lat))
dev.off()

tiff('air.trend.tiff', width=1800, height=1800, res=300, compression='lzw')

my.mat = interp(all.slopes$lon, all.slopes$lat, all.slopes$air.slope)
#tiff('kd.area.slope.heat.tiff', width=1800, height=1200, res=300, compression='lzw')
filled.contour(my.mat, color.palette=
                 colorRampPalette(c("violet","blue","cyan", "green3", "yellow", "orange", "red"), 
                  bias = 1, space = "rgb"), ylab="Lat", xlab="Lon", main='Air Trend',
                  plot.axes =  lines(wi$Lon, wi$Lat))
dev.off()

tiff('sw.trend.tiff', width=1800, height=1800, res=300, compression='lzw')

my.mat = interp(all.slopes$lon, all.slopes$lat, all.slopes$sw.slope)
#tiff('kd.area.slope.heat.tiff', width=1800, height=1200, res=300, compression='lzw')
filled.contour(my.mat, color.palette=
                 colorRampPalette(c("violet","blue","cyan", "green3", "yellow", "orange", "red"), 
                                  bias = 1, space = "rgb"), ylab="Lat", xlab="Lon", main='SW Trend',
               plot.axes =  lines(wi$Lon, wi$Lat))

dev.off()

tiff('lw.trend.tiff', width=1800, height=1800, res=300, compression='lzw')

my.mat = interp(all.slopes$lon, all.slopes$lat, all.slopes$lw.slope)
#tiff('kd.area.slope.heat.tiff', width=1800, height=1200, res=300, compression='lzw')
filled.contour(my.mat, color.palette=
                 colorRampPalette(c("violet","blue","cyan", "green3", "yellow", "orange", "red"), 
                                  bias = 1, space = "rgb"), ylab="Lat", xlab="Lon", main='LW Trend',
               plot.axes =  lines(wi$Lon, wi$Lat))

dev.off()

my.mat = interp(all.slopes$lon, all.slopes$lat, all.slopes$ws.slope)
#tiff('kd.area.slope.heat.tiff', width=1800, height=1200, res=300, compression='lzw')
filled.contour(my.mat, color.palette=
                 colorRampPalette(c("violet","blue","cyan", "green3", "yellow", "orange", "red"), 
                                  bias = 1, space = "rgb"), ylab="Lat", xlab="Lon", main='Wind Trend',
               plot.axes =  lines(wi$Lon, wi$Lat))


#dev.off()
