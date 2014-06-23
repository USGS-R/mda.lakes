
#all.slopes = fread('all.slopes.csv')
source('../R/Libraries/GLM.functions.R')
all.slopes = fread('all.slopes.csv')
wi = fread('../supporting files/WI_boundary_lat_lon.tsv')

u.wbic = unique(all.slopes$wbic)

lat.lon = matrix(NA, nrow=length(u.wbic), ncol=2)

for(i in 1:length(u.wbic)){
	lat.lon[i,] = getLatLon(as.character(u.wbic[i]))
}

tiff('grl.figure.1.tiff', width=1600, height=2100, res=300, compression='lzw')
plot(wi$Lon, wi$Lat, type='l', lwd=2, bty='n', ylab='Lat', xlab='Lon', col='grey')
points(lat.lon[,2], lat.lon[,1], bg = 'blue', col='grey', pch=21)
dev.offf()