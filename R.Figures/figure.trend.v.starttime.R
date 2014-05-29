library(plyr)
library(emdbook)
source('../R/Libraries/GLM.functions.R')

data = read.table('../Output/omg.huge.output.tsv', sep='\t', header=TRUE)

air.temp = read.table('../Output/airtemp.metrics.csv', sep=',', header=TRUE)

all.data = merge(air.temp, data, by.x=c("WBIC", "Year"), by.y=c("lakeid", "year"))

jas.air.slopes = ddply(all.data, c('WBIC'), function(df) lm(df$JAS.Mean ~ df$Year)$coeff[2])

jas.surf.slopes = ddply(all.data, c('WBIC'), function(df) lm(df$mean_surf_JAS ~ df$Year)$coeff[2])
names(jas.surf.slopes) = c('WBIC','surf.slope')
names(jas.air.slopes) = c('WBIC','air.slope')


start.year = 1979:1995
air.med.slope = rep(NA, length(start.year))
wtr.med.slope = rep(NA, length(start.year))

for(i in 1:length(start.year)){
  y = start.year[i]
  jas.air.slopes = ddply(all.data[all.data$Year >= y,], c('WBIC'), function(df) lm(df$JAS.Mean ~ df$Year)$coeff[2])
  jas.surf.slopes = ddply(all.data[all.data$Year >= y,], c('WBIC'), function(df) lm(df$mean_surf_JAS ~ df$Year)$coeff[2])
  air.med.slope[i] = median(jas.air.slopes$`df$Year`, na.rm=TRUE)
  wtr.med.slope[i] = median(jas.surf.slopes$`df$Year`, na.rm=TRUE)
}

tiff('trend.v.startyear.tiff', width=2400, height=1200, res=300, compression='lzw')
plot(start.year, air.med.slope, type='b', ylim=c(0,0.14), ylab="Median Slope (°C/Year)", xlab='Year',
     lwd=2)
lines(start.year, wtr.med.slope, type='b', col='blue', lwd=2)
legend('topleft',c('Air','Water'), col=c('black','blue'),  pch=21, lty=1)

dev.off()
