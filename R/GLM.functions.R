getBathy	<-	function(WBIC){
	
	return(bathymetry)
}

getArea	<-	function(WBIC){
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

getLatLon	<-	function(WBIC){
	# hmmm...Luke?
	return(latitude.longitude)
}

getWstr	<-	function(WBIC){
	# Markfort et al. 2010
	hv	<-	max(c(getCanopy(WBIC),1)
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
	return(max.depth)
}


