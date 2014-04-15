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

getResidenceTime	<-	local(
	{ lookup=NULL 
		
		default.RT	<-	157.2 # days
		
		function(WBIC,default.if.null=FALSE) {
			if (is.null(lookup)) { 
				cat('Caching residence time info.\n')
				d	<-	read.table('../supporting files/Res.time.Diebel.csv',
					header=TRUE,sep=',')
				lookup <<- new.env()
				for (i in 1:nrow(d)){
					lookup[[toString(d$WBIC[i])]]	<-	d$med.RT.days[i]
				}
			}
			wbic.val = lookup[[as.character(WBIC)]]

			if (is.null(wbic.val) & default.if.null==TRUE){
			  return(default.RT)
			} else if (is.null(wbic.val) & default.if.null==FALSE){
			  return(NA)
			} else {
			  return(wbic.val)
			}
		}
	}
)

getCanopy	<-	local(
	{ lookup=NULL 
		
		default.hc	<-	0.5
		
		function(WBIC,default.if.null=FALSE,method="ASTER") {
			if (is.null(lookup)) { 
        if (method=='ASTER'){
          cat('Caching canopy info.\n')
          d	<-	read.table('../supporting files/canopyht_zonal_no_zero_num5_positivehts.csv',
                          header=TRUE,sep=',')
          lookup <<- new.env()
          
          for (i in 1:nrow(d)){
            lookup[[toString(d$WBIC[i])]]	<-	d[i,2]
          }
        } else if (method=="landcover"){
          cat('Caching landcover info.\n')
          d  <-	read.table('../supporting files/buffers_land_cover.csv',
                           header=TRUE,sep=',')
          lookup <<- new.env()
          
          for (i in 1:nrow(d)){
            #100 urban
            #110 ag
            #150 grassland
            #160 forest 
            #200 open water
            #210 wetland
            table.lc = data.frame("lc_200"=0.5,"lc_160"=11.5,"lc_100"=0.5,"lc_150"=0.65,"lc_110"=0.8,"lc_210"=0.5)
            val = table.lc[paste("lc_",as.character(d[i,2]),sep='')]
            lookup[[toString(d$WBDY_WBIC[i])]]	<-	as.numeric(val)
          }
        }
				
			}
			wbic.val = lookup[[as.character(WBIC)]]

			if (is.null(wbic.val) & default.if.null==TRUE){
			  return(default.hc)
			} else if (is.null(wbic.val) & default.if.null==FALSE){
			  return(NA)
			} else {
			  return(wbic.val)
			}
		}
	}
)

getScenarioKd <- function(WBIC,years,year.1=1979,year.2=2011,trend=0,default.if.null=FALSE){
  #WBIC is a string
  #years is a single numeric or vector of numerics
  #year.1 and year.2 are the bounds of the averaging (i.e., values outside of this range will not be used)
  #trend is a percentage of SECCHI increase (positive number) or decrease (negative number). E.g., 0.94 is a 0.94%/yr increase in SECCHI (decrease in Kd) . 
  #default.if.null is boolean. If default.if.null==T, a default kd will be used (centered) and the trend applied
  
  if (is.na(WBIC)){stop('WBIC cannot be NA')}
  if (is.null(WBIC)){stop('WBIC cannot be NULL')}
  
  default.kd  <-	0.6983965
  
  secchiConv  <-	1.7
  
  d	<-	read.table('../supporting files/annual_mean_secchi.txt',
                  header=TRUE,sep='\t')
  
  useI  <-	d$WBIC==WBIC
  Kd <- vector(length=length(years))
  
  if (!any(useI) & default.if.null==T){
    year.cent = year.1  #mean(c(year.1,year.2))
    secchi.mn = secchiConv/default.kd

  } else if (!any(useI) & default.if.null==F){
    
    return(NULL)    
  } else if (any(useI) & all(is.na(d[useI,'year']))){
	  warning('No date info, using beginning of simulation as pivot point.')
	  dat.WBIC <- d[useI,]
    
    year.cent <- year.1
    secchi.mn <- mean(dat.WBIC$secchi.m.mean) # mean at pivot point!
	
  } else {
    dat.WBIC <- d[useI,]
    
    yr.i = dat.WBIC$year >= year.1 & year.2 >= dat.WBIC$year
    
    year.cent <- year.1  #mean(dat.WBIC$year[yr.i]) # the pivot point!
    secchi.mn <- mean(dat.WBIC$secchi.m.mean[yr.i]) # mean at pivot point!
  }
  
    #sech = m*x+b
    m = trend*secchi.mn*0.01
    #solve for b:
    b= secchi.mn-m*year.cent
    #convert to Kd:
    
  for (i in 1:length(years)){
    Kd[i] <- secchiConv/(m*years[i]+b)
  }
    
  return(Kd)
}

getClarity	<-	local(
	{ lookup=NULL
		
		default.kd	<-	0.6983965
		
		function(WBIC,default.if.null=FALSE){
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
			wbic.val = lookup[[as.character(WBIC)]]

			if (is.null(wbic.val) & default.if.null==TRUE){
				return(default.kd)
			} else if (is.null(wbic.val) & default.if.null==FALSE){
			  return(NA)
			} else {
        return(wbic.val)
			}
		}
	}
)
#Return perimeter in meters
getElevation <- local({ lookup=NULL; function(WBIC) {
  if (is.null(lookup)) { 
    cat('Caching elevation info\n')
    d <-  read.table('../supporting files/WI_ManagedLakes_elevation.tsv',header=TRUE,sep='\t')
    WBIC.names= names(d[-1]) # remove first col, it is junk
    lookup <<- new.env()
    for(i in 1:length(WBIC.names)){
      names.W = WBIC.names[i] # need to remove the "X"
      lookup[[toString(substr(x=names.W,2,stop=nchar(names.W)))]] = as.numeric(levels(d[1,names.W])[1])
    }
  }
  wbic.val = lookup[[as.character(WBIC)]]
  
  if (is.null(wbic.val)){
    return(NA)
  } else {
    return(wbic.val)
  }
}})

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
  if (length(SDF)==0){
    return(NA)
  } else {
    return(SDF)
  }

}
getCD	<-	function(WBIC=NULL,Wstr=NULL){
	if (is.null(WBIC) & is.null(Wstr)){
		stop('either WBIC or Wstr need to be defined')
	} else if (is.null(Wstr)){
		Wstr	<-	getWstr(WBIC)
	}
	
	coef_wind_drag.ref	<-	0.00140
	coef_wind_drag	<-	coef_wind_drag.ref*Wstr^0.33
	return(coef_wind_drag)
}

getWstr	<-	function(WBIC,method='Markfort',canopy=NULL){
	# Markfort et al. 2010
	minWstr	<-	0.0001
	if (is.null(canopy)){
		hc	<-	max(c(getCanopy(WBIC),1))
	} else {
		hc	<-	canopy
	}
	
	lkeArea	<-	getArea(WBIC)
  
  if(is.na(lkeArea)){
    return(NA)
  }

	Xt	<-	50*hc
	
	
	if (method=='Markfort'){
		D	<-	2*sqrt(lkeArea/pi)
		if (D<Xt){
			wind.shelter	<-	minWstr
		} else {
			wind.shelter	<-	2/pi*acos(Xt/D)-(2*Xt/(pi*D^2))*sqrt(D^2-Xt^2)
		}
	} else if (method=="Hondzo") {
		lkeArea.km2 = lkeArea*1.0e-6 # to km2
		wind.shelter= 1.0 - exp(-0.3*lkeArea.km2)
	} else {
		perim	<-	getPerim(WBIC)
		shelArea	<-	perim*hc*12.5 # 25% of Markfort, as sheltering is single direction...
		shelter	<-	(lkeArea-shelArea)/lkeArea
		wind.shelter	<-	max(c(shelter,minWstr))
		if (is.null(perim)){wind.shelter<-NULL}
	}
	
	return(wind.shelter)
}


getZmax <- local({ lookup=NULL; function(WBIC) {
  if (is.null(lookup)) { 
    cat('Caching depth info.\n')
    
    d	<-	read.table('../supporting files/managed_lake_info.txt',header=TRUE,sep='\t',quote="\"")
    
    lookup <<- new.env()
    ft2m  <-  0.3048
    for(i in 1:nrow(d)){
      mean.m <- d$max.depth.ft[i]
      lookup[[toString(d$WBIC[i])]] = mean(ft2m*mean.m)
    }
  }
  lookup[[WBIC]]
}})


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
    	if (!any(use.i)){
      		# not found for this year
      		use.i  <-	WBIC[j]==empir.ice$WBIC & empir.ice$ON.OFF=="on" & (
        		substr(empir.ice$DATE,1,4)==as.character(year+1)) 
			if (any(use.i)){
				pos.results  <-  empir.ice$DATE[use.i]
			} else {pos.results=NA}
      		
      		ice.on[j]<- pos.results[1]
    	} else {
      		# found for this year, but could be 2 matches
      		pos.results  <-	empir.ice$DATE[use.i]
      		ice.on[j]<- tail(pos.results,1)
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
		if (any(use.i)){
			pos.results	<-	empir.ice$DATE[use.i]
			ice.off[j]	<-	pos.results[1] # warn if length(pos.results)==2?
		} else { 
			ice.off[j]	<-	NA
		}
		
	}
	return(ice.off)
}
