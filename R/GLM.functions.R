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

getArea	<-	local({ lookup=NULL; function(WBIC){
	if (is.null(lookup)) { 
		cat('Caching area info.\n')
		acre2m2	<-	4046.85642
		d	<-	read.table('../supporting files/managed_lake_info.txt',
			header=TRUE,sep='\t',quote="\"")
		lookup <<- new.env()
		
		for (i in 1:nrow(d)){
			lookup[[toString(d$WBIC[i])]]	<-	acre2m2*d$acres[i]
		}
	}
	lookup[[WBIC]]
}})

getCanopy	<-	local({ lookup=NULL; function(WBIC) {
	if (is.null(lookup)) { 
		cat('Caching canopy info.\n')
		d	<-	read.table('../supporting files/canopyht_zonal_no_zero_num5_positivehts.csv',
			header=TRUE,sep=',')
		lookup <<- new.env()
		
		for (i in 1:nrow(d)){
			lookup[[toString(d$WBIC[i])]]	<-	d[i,2]
		}
	}
	lookup[[WBIC]]
}})


getClarity	<-	local({ lookup=NULL; function(WBIC){
	if (is.null(lookup)){
		cat('Caching clarity info.\n')
		secchiConv	<-	1.7
		d	<-	read.table('../supporting files/annual_mean_secchi.txt',
			header=TRUE,sep='\t')
		lookup <<- new.env()
		unWBIC	<-	unique(as.character(d$WBIC))
		for (i in 1:length(unWBIC)){
			useI	<-	d$WBIC==unWBIC[i]
			secchi 	<-	d$secchi.m.mean[useI]
			attenuation.coefficient	<-	secchiConv/mean(secchi,na.rm=TRUE)
			lookup[[unWBIC[i]]] 	<- attenuation.coefficient
		}
	}
	lookup[[WBIC]]
}})


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


getSDF	<-	function(WBIC){
	perim	<-	getPerim(WBIC)
	area	<-	getArea(WBIC)
	circle.perim	<-	2*pi*sqrt(area/pi)
	SDF	<-	perim/circle.perim
	return(SDF)
}


getWstr	<-	function(WBIC,method='Markfort'){
	# Markfort et al. 2010
	minWstr	<-	0.0001
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
		shelArea	<-	perim*hc*12.5 # 25% of Markfort, as sheltering is single direction...
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

getIceOn	<-	function(WBIC,year){
  early.freeze	<-	8	# month of year
	# the ice on for each lake for a given year
	# ice on is assumed to either happen during the same calendar year, or within the NEXT year
	empir.ice = read.table('../supporting files/empirical.ice.tsv', sep='\t', header=TRUE, as.is=TRUE) 
	ice.on	<-	vector(length=length(WBIC))
	for (j in 1:length(WBIC)){
		use.i	<-	WBIC[j]==empir.ice$WBIC & empir.ice$ON.OFF=="on" & (
			substr(empir.ice$DATE,1,4)==as.character(year)) 
    if (!any(use.i))
    {
      # not found for this year
      use.i  <-	WBIC[j]==empir.ice$WBIC & empir.ice$ON.OFF=="on" & (
        substr(empir.ice$DATE,1,4)==as.character(year+1)) 
      pos.results  <-  empir.ice$DATE[use.i]
      ice.on<- pos.results[1]
    } else {
      # found for this year, but could be 2 matches
      pos.results  <-	empir.ice$DATE[use.i]
      ice.on<- tail(pos.results,1)
    }

	}
	return(ice.on)
}

getIceOff	<-	function(WBIC,year) {
	# the ice off for each lake for a given year
	# ice off is assumed to happen during the same calendar year

	empir.ice	<-	read.table('../supporting files/empirical.ice.tsv', sep='\t', header=TRUE, as.is=TRUE)
	ice.off	<-	vector(length=length(WBIC))
	for (j in 1:length(WBIC)){
		use.i	<-	WBIC[j]==empir.ice$WBIC & empir.ice$ON.OFF=="off" & substr(empir.ice$DATE,1,4)==as.character(year)
		ice.off[j]	<-	empir.ice$DATE[use.i]
	}
	return(ice.off)
}
