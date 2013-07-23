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
	return(elevation)
}

getLatitude	<-	function(WBIC){
	return(latitude)
}

getLongitude<-	function(WBIC){
	return(longitude)
}

getWstr	<-	function(WBIC){
	return(wind.shelter)
}

getZmax	<-	function(WBIC){
	return(max.depth)
}


