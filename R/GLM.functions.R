getBathy	<-	function(WBIC){
	numZ	<-	15
	fileN	<-	paste(c('../supporting files/Bathy/',WBIC,'.bth'),collapse='')
	if (file.exists(fileN)){
		data	<-	read.table(fileN,header=TRUE,sep='\t')
		bathymetry	<-	data.frame(depth=data$depth,area=data$area)
	} else {
		lkeArea	<-	getArea(WBIC)
		zMax	<-	getZmax(WBIC)
		depth	<-	seq(0,zMax,length.out=numZ)
		area	<-	approx(c(0,zMax),c(lkeArea,0),depth)$y
		bathymetry	<-	data.frame(depth=depth,area=area)
	}
	
	return(bathymetry)
}

getArea	<-	function(WBIC){
	acre2m2	<-	4046.85642
	data	<-	read.table('../supporting files/managed_lake_info.txt',header=TRUE,sep='\t',quote="\"")
	useI	<- data$WBIC==as.numeric(WBIC)
	if (any(useI)){
		surface.area	<-	acre2m2*mean(data$acres[useI],na.rm=TRUE)
	} else {
		surface.area	<-	NULL
	}
	
	return(surface.area)
}

getCanopy	<-	function(WBIC){
	data	<-	read.table('../supporting files/VegetationHeight_WiLMA.tsv',header=TRUE,sep='\t')
	indx	<-	paste(c('X',WBIC),collapse='')
	if (indx %in% names(data)){
		canopy.height	<-	as.numeric(levels(data[1,indx])[1])
	} else {
		canopy.height	<-	NULL
	}
	return(canopy.height)
}

getClarity	<-	function(WBIC){
	secchiConv	<-	1.7
	data	<-	read.table('../supporting files/annual_mean_secchi.txt',header=TRUE,sep='\t')
	useI	<- data['WBIC']==WBIC
	if (any(useI)){
		secchi 	<-	data$secchi.m.mean[useI]
		attenuation.coefficient	<-	secchiConv/mean(secchi,na.rm=TRUE)
	} else {
		attenuation.coefficient	<-	NULL
	}
	return(attenuation.coefficient)
}

getElevation	<-	function(WBIC){
	data	<-	read.table('../supporting files/WI_ManagedLakes_elevation.tsv',header=TRUE,sep='\t')
	indx	<-	paste(c('X',WBIC),collapse='')
	if (indx %in% names(data)){
		elevation	<-	as.numeric(levels(data[1,indx])[1])
	} else {
		elevation	<-	NULL
	}
	return(elevation)
}

#This uses a little fanciness to prevent loading file on every call.
getLatLon <- local({ lookup=NULL; function(WBIC) {
	if (is.null(lookup)) { 
		cat('Caching lat/lon info.\n')
		d <- read.table('../supporting files/WI_Lakes_WbicLatLon.tsv', header=TRUE, as.is=TRUE) 
		lookup <<- new.env()
		
		for(i in 1:nrow(d)){
			lookup[[toString(d$WBIC[i])]] = c(d$LAT[i], d$LON[i])
		}
	}
	lookup[[WBIC]]
}})


getWstr	<-	function(WBIC){
	# Markfort et al. 2010
	hc	<-	max(c(getCanopy(WBIC),1))
	lkeArea	<-	getArea(WBIC)
	D	<-	2*sqrt(lkeArea/pi)
	Xt	<-	50*hc
	
	if (D<Xt){
		wind.shelter	<-	0.01
	} else {
		wind.shelter	<-	2/pi*acos(Xt/D)-(2*Xt/(pi*D^2))*sqrt(D^2-Xt^2)
	}
	return(wind.shelter)
}

getZmax	<-	function(WBIC){
	ft2m	<-	0.3048
	data	<-	read.table('../supporting files/managed_lake_info.txt',header=TRUE,sep='\t',quote="\"")
	useI	<- data$WBIC==as.numeric(WBIC)
	if (any(useI)){
		max.depth	<-	ft2m*mean(data$max.depth.ft[useI],na.rm=TRUE)
	} else {
		max.depth	<-	NULL
	}
	
	return(max.depth)
}

getZmean	<-	function(WBIC){
	ft2m	<-	0.3048
	data	<-	read.table('../supporting files/managed_lake_info.txt',header=TRUE,sep='\t',quote="\"")
	useI	<- data$WBIC==as.numeric(WBIC)
	if (any(useI)){
		mean.depth	<-	ft2m*mean(data$mean.depth.ft[useI],na.rm=TRUE)
	} else {
		mean.depth	<-	1/3*getZmax(WBIC)
	}
	
	return(mean.depth)
}




