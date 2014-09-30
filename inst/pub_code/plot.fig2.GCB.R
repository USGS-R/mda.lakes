require(dataRetrieval)
require(maps)
require(mapdata)
require(rjson)
require(scales)
require(raster)

reload.ftp = F

alaska.fix = c(-35,1.5,-2800000,-2600000)
hawaii.fix = c(-35,1,6800000,-1600000)
st.date = '2012-06-01'
en.date = '2012-08-01'
colors = c('firebrick','green','blue')
plot.map = T


require(rgdal)
require(maptools)


us = readOGR(dsn = "states_21basic",layer="states")
usa = us[! us$STATE_NAME %in% c("Alaska","Hawaii"),]
usAEA = spTransform(usa,CRS("+init=epsg:2163"))
#
#usfix = fixup(usAEA,alaska.fix,hawaii.fix)
#plot(usfix)


library(fields)

if (reload.ftp){
  ftp.string <- "ftp://hydro1.sci.gsfc.nasa.gov/data/s4pa/NLDAS/NLDAS_FORA0125_H.002/1979/152/NLDAS_FORA0125_H.A19790601.0000.002.grb"
  #this link may become broken with time, as folders are removed after some time. just edit the date to reflect the most recent day at the time you run these lines
  
  download.file(ftp.string, "foo.grb", mode="wb")
}


grib <- readGDAL("foo.grb")
is.na(grib$band1) <- grib$band1 > 100

offset.xy = c(24000,0)

grib2 <- spTransform(grib,CRS("+init=epsg:2163"))
grib.offs = elide(grib2[,1],shift=offset.xy)

data.scale <- grib2[, 1]@data[[1]] 
# get as integers on 1-100 scale
range = max(data.scale,na.rm=T) - min(data.scale,na.rm=T)
data.scale = data.scale*(100/range)
data.scale = floor(data.scale-min(data.scale,na.rm=T))

jet.colors <-
  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))(100)

CRS.proj=proj4string(us)

## lat lines!!!
lat.1 = c(-117,30)
lat.2 = c(-125,40)
lat.3 = c(-126,50)

long.vals <- rep(x=lat.1[2],length.out=100)
lat.vals <- seq(from=lat.1[1],to=-80,length.out=100)
lat.dat = Line( matrix(data=c(lat.vals,long.vals),nrow=100,ncol=2))
lines.30 <- Lines(lat.dat,ID='a')
long.vals <- rep(x=lat.2[2],length.out=100)
lat.vals <- seq(from=lat.2[1],to=-73,length.out=100)
lat.dat = Line( matrix(data=c(lat.vals,long.vals),nrow=100,ncol=2))
lines.40 <- Lines(lat.dat,ID='b')
long.vals <- rep(x=lat.3[2],length.out=100)
lat.vals <- seq(from=lat.3[1],to=-66,length.out=100)
lat.dat = Line( matrix(data=c(lat.vals,long.vals),nrow=100,ncol=2))
lines.50 <- Lines(lat.dat,ID='c')
lats = SpatialLines(LinesList= list("line30"=lines.30,
                                    "line40"=lines.40,
                                    "line50"=lines.50),
                    CRS(CRS.proj)) #proj4string(us))

lats.conv = spTransform(lats,CRS("+init=epsg:2163"))

lat.txt.1 <- SpatialPoints(coords= matrix(data=lat.1,nrow=1,ncol=2),
                          CRS(proj4string(us)))
lat.txt.1 <- spTransform(lat.txt.1,CRS("+init=epsg:2163"))  
lat.txt.2 <- SpatialPoints(coords= matrix(data=lat.2,nrow=1,ncol=2),
                       CRS(proj4string(us)))
lat.txt.2 <- spTransform(lat.txt.2,CRS("+init=epsg:2163"))
lat.txt.3 <- SpatialPoints(coords= matrix(data=lat.3,nrow=1,ncol=2),
                       CRS(proj4string(us)))
lat.txt.3 <- spTransform(lat.txt.3,CRS("+init=epsg:2163"))

## long lines!!
lon.1 = c(-85,53.5)
lon.2 = c(-100,53.5)
lon.3 = c(-115,53.5)

long.vals <- rep(x=lon.1[1],length.out=100)
lat.vals <- seq(from=lon.1[2],to=29,length.out=100)
lon.dat = Line( matrix(data=c(long.vals,lat.vals),nrow=100,ncol=2))
lines.100 <- Lines(lon.dat,ID='d')
long.vals <- rep(x=lon.2[1],length.out=100)
lat.vals <- seq(from=lon.2[2],to=24.5,length.out=100)
lon.dat = Line( matrix(data=c(long.vals,lat.vals),nrow=100,ncol=2))
lines.110 <- Lines(lon.dat,ID='e')

long.vals <- rep(x=lon.3[1],length.out=100)
lat.vals <- seq(from=lon.3[2],to=27,length.out=100)
lon.dat = Line( matrix(data=c(long.vals,lat.vals),nrow=100,ncol=2))
lines.120 <- Lines(lon.dat,ID='f')

lons = SpatialLines(LinesList= list("line100"=lines.100,"line110"=lines.110,"line120"=lines.120),
                    CRS(CRS.proj)) #proj4string(us))
lons.conv = spTransform(lons,CRS("+init=epsg:2163"))
lon.txt.1 <- SpatialPoints(coords= matrix(data=lon.1,nrow=1,ncol=2),
                           CRS(proj4string(us)))
lon.txt.1 <- spTransform(lon.txt.1,CRS("+init=epsg:2163"))  
lon.txt.2 <- SpatialPoints(coords= matrix(data=lon.2,nrow=1,ncol=2),
                           CRS(proj4string(us)))
lon.txt.2 <- spTransform(lon.txt.2,CRS("+init=epsg:2163"))
lon.txt.3 <- SpatialPoints(coords= matrix(data=lon.3,nrow=1,ncol=2),
                           CRS(proj4string(us)))
lon.txt.3 <- spTransform(lon.txt.3,CRS("+init=epsg:2163"))



plot(grib.offs,cex=.5,col=jet.colors[data.scale],pch=15)

#par(new=TRUE)
plot(usAEA,border="grey60",add=TRUE, lwd=4)

plot(lats.conv,add=TRUE, lwd=4,lty=5)
plot(lons.conv,add=TRUE, lwd=4,lty=5)

text(x=lat.txt.1,labels='30°N',pos=2,cex=3)
text(x=lat.txt.2,labels='40°N',pos=2,cex=3)
text(x=lat.txt.3,labels='50°N',pos=2,cex=3)

text(x=lon.txt.1,labels='85°W',pos=3,cex=3)
text(x=lon.txt.2,labels='100°W',pos=3,cex=3)
text(x=lon.txt.3,labels='115°W',pos=3,cex=3)
