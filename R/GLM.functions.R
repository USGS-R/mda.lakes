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
	data	<-	read.table('../supporting files/canopyht_zonal_no_zero_num5_positivehts.csv',header=TRUE,sep=',')
	useI	<-	which(data[,1]==WBIC)
	if(length(useI)>0){
		canopy.height	<-	as.numeric(data[useI,2])
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

#Return perimeter in meters
getPerim <- local({ lookup=NULL; function(WBIC) {
	if (is.null(lookup)) { 
		cat('Caching perimeter info.\n')
		d <- read.table('../supporting files/wbicAreaPerim.tsv', header=TRUE, as.is=TRUE) 
		lookup <<- new.env()
		
		for(i in 1:nrow(d)){
			lookup[[toString(d$WBIC[i])]] = d$PERIMETER[i]
		}
	}
	lookup[[WBIC]]
}})


getWstr	<-	function(WBIC,method='Markfort'){
	# Markfort et al. 2010
	minWstr	<-	0.001
	hc	<-	max(c(getCanopy(WBIC),1))
	lkeArea	<-	getArea(WBIC)

	Xt	<-	50*hc
	
	
	if (method=='Markfort'){
		D	<-	2*sqrt(lkeArea/pi)
		if (D<Xt){
			wind.shelter	<-	minWstr
		} else {
			wind.shelter	<-	2/pi*acos(Xt/D)-(2*Xt/(pi*D^2))*sqrt(D^2-Xt^2)
		}
	} else {
		perim	<-	getPerim(WBIC)
		shelArea	<-	0.2*perim*hc*50 # 25% of Markfort, as sheltering is single direction...
		shelter	<-	(lkeArea-shelArea)/lkeArea
		wind.shelter	<-	max(c(shelter,minWstr))
		if (is.null(perim)){wind.shelter<-NULL}
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
	mean.depth	<-	NA
	if (any(useI)){
		mean.depth	<-	ft2m*mean(data$mean.depth.ft[useI],na.rm=TRUE)
	}
	if (is.na(mean.depth)){
		mean.depth	<-	1/3*getZmax(WBIC)
	}
	return(mean.depth)
}




