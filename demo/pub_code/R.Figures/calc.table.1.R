
source('../R/Libraries/GLM.functions.R')
tmp = fread('downsampled.slopes.csv')

lakes = unique(tmp[,list(wbic,area,kd,zmax)])

lakes[,elev:=NaN]
lakes[,lat:=NaN]
lakes[,lon:=NaN]

for(i in 1:nrow(lakes)){
	lakes$elev[i] = getElevation(as.character(lakes$wbic[i]))
	latlon = getLatLon(as.character(lakes$wbic[i]))
	lakes$lat[i] = latlon[1]
	lakes$lon[i] = latlon[2]
}




