#'@title Get Hypsometry for a given lake
#'@description
#'Returns the hypsometry profile for a lake with the given ID
#'
#'@param site_id The character ID for the requested data
#'
#'@return
#' 
#'@details
#'
#'
#'
#'@author 
#'Luke Winslow, Jordan Read
#'
#'@examples
#'
#'
#'@export
getBathy	<-	function(site_id){
	numZ	<-	15
	fileN	<-	system.file(paste(c('supporting_files/Bathy/', site_id, '.bth'), collapse=''), 
											 package=getPackageName())
	if (file.exists(fileN)){
		data	<-	read.table(fileN,header=TRUE,sep='\t')
		bathymetry	<-	data.frame(depth=data$depth,area=data$area)
	} else {
		lkeArea	<-	getArea(site_id)
		zMax	<-	getZmax(site_id)
		depth	<-	seq(0,zMax,length.out=numZ)
		area	<-	approx(c(0,zMax),c(lkeArea,0),depth)$y
		bathymetry	<-	data.frame(depth=depth,area=area)
	}
	
	return(bathymetry)
}

#'@title Get lake surface area 
#'@description
#'Returns the surface area for a lake with given ID
#'
#'@param site_id The character ID for the requested data
#'
#'@return
#' Lake surface area in meters^2
#' 
#'@details
#'
#'
#'
#'@author 
#'Jordan Read, Luke Winslow
#'
#'@examples
#'
#'
#'@export
getArea	<-	local({ lookup=NULL; function(site_id){
	if (is.null(lookup)) { 
		cat('Caching area info.\n')
		acre2m2	<-	4046.85642
		fname = system.file('supporting_files/managed_lake_info.txt', package=getPackageName())
		d	<-	read.table(fname, header=TRUE, sep='\t', quote="\"")
		lookup <<- new.env()
		
		for (i in 1:nrow(d)){
			lookup[[toString(d$site_id[i])]]	<-	acre2m2*d$acres[i]
		}
	}
	lookup[[site_id]]
}})

#'@title Get estimated lake residence time
#'@description
#'Returns the estimated residence time for a lake with the given ID
#'
#'@param site_id The character ID for the requested data
#'
#'@return
#' Estimated residence time in days
#'@details
#'
#'
#'@references
#'TODO: Data source needed
#'
#'@author 
#'Luke Winslow, Jordan Read
#'
#'@examples
#'
#'
#'@export
getResidenceTime	<-	local(
	{ lookup=NULL 
		
		default.RT	<-	157.2 # days
		
		function(WBIC,default.if.null=FALSE) {
			if (is.null(lookup)) { 
				cat('Caching residence time info.\n')
				fname = system.file('supporting_files/Res.time.Diebel.csv', package=getPackageName())
				d	<-	read.table(fname, header=TRUE, sep=',')
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

#'@title Get surrounding canopy height for a given lake
#'@description
#'Returns the surrounding canopy height for a lake with the given ID
#'
#'@param site_id The character ID for the requested data
#'@param method Canopy height estimation method [aster or landcover]
#'@param default.if.null Default value to return if canopy height is unknown
#'
#'@return
#' Canopy height above lake surface level in meters
#'@details
#'
#'
#'
#'@author 
#'Luke Winslow, Jordan Read
#'
#'@examples
#'
#'
#'@export
getCanopy	<-	local(
	{ lookup=NULL 
		
		default.hc	<-	0.5
		
		function(WBIC,default.if.null=FALSE,method="aster") {
			if (is.null(lookup)) { 
        if (tolower(method) == 'aster'){
          cat('Caching canopy info.\n')
          fname = system.file('supporting_files/canopyht_zonal_no_zero_num5_positivehts.csv', 
          										package =getPackageName())
          d	<-	read.table(fname, header=TRUE, sep=',')
          lookup <<- new.env()
          
          for (i in 1:nrow(d)){
            lookup[[toString(d$WBIC[i])]]	<-	d[i,2]
          }
        } else if (tolower(method) == "landcover"){
          cat('Caching landcover info.\n')
          system.file('supporting_files/buffers_land_cover.csv', package=getPackageName())
          d  <-	read.table(fname, header=TRUE, sep=',')
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
#'@title Get light attenuation based on long-term trend scenario for a given lake
#'@description
#'Calculate the long-term values of light attenuation for a given lake based on
#'changing scenario
#'
#'@param site_id The character ID for the requested data
#'@param years Numeric year vector across which to calculate Kd values
#'@param year.1 The numeric year bounds of the averaging
#' (i.e., values outside of this range will not be used)
#'@param trend The percentage of Secchi increase per year. 
#'Positive is increasing secchi (e.g., 0.94 is 0.94% increase per year)
#'@param default.if.null boolean indicating if default Kd should be used if lake has no observations
#'@return
#' light attenuation coefficient in m^-1
#' 
#'@details
#'
#'
#'
#'@author 
#'Luke Winslow, Jordan Read
#'
#'@examples
#'
#'
#'@export
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
  fname = system.file('supporting_files/annual_mean_secchi.txt', package=getPackageName())
  d	<-	read.table(fname, header=TRUE, sep='\t')
  
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

#'@title Get light attenuation coefficient for a given lake
#'@description
#'Returns the light attenuation coefficient for a lake with the given ID
#'
#'@param site_id The character ID for the requested data
#'@param default.if.null boolean indicating if default Kd should be used if lake has no observations
#'
#'@return
#' Light attenuation coefficient in m^-1
#'@details
#'
#'
#'
#'@author 
#'Luke Winslow, Jordan Read
#'
#'@examples
#'
#'
#'@export
getClarity	<-	local(
	{ lookup <- NULL
		default.kd	<-	0.6983965
		
		function(WBIC, default.if.null=FALSE){
			if (is.null(lookup)){
				cat('Caching clarity info.\n')
				secchiConv	<-	1.7
				
				fname <- system.file('supporting_files/annual_mean_secchi.txt', package=getPackageName())
				d	<-	read.table(fname, header=TRUE, sep='\t')
				
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

#'@title Get surface elevation for a given lake
#'@description
#'Get the elevation of a lake with given ID
#'
#'@param site_id The character ID for the requested data
#'
#'@return
#' Elevation in meters
#'@details
#'
#'
#'
#'@author 
#'Luke Winslow, Jordan Read
#'
#'@examples
#'
#'
#'@export
getElevation <- local({ lookup=NULL; 
	function(WBIC) {
	  if (is.null(lookup)) { 
	    cat('Caching elevation info\n')
	    fname <- system.file('supporting_files/WI_ManagedLakes_elevation.tsv', package=getPackageName())
	    d <-  read.table(fname, header=TRUE, sep='\t')
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

#'@title Get latitude and longitude for a given lake
#'@description
#'Get the center lat/lon of a lake with given ID
#'
#'@param site_id The character ID for the requested data
#'
#'@return
#' Lat/lon on the WGS84 datum
#' 
#'@details
#'
#'
#'
#'@author 
#'Luke Winslow, Jordan Read
#'
#'@examples
#'
#'
#'@export
getLatLon <- local({ lookup=NULL; function(WBIC) {
	if (is.null(lookup)) { 
		cat('Caching lat/lon info.\n')
		
		fname <- system.file('supporting_files/WI_Lakes_WbicLatLon.tsv', package=getPackageName())
		d <- read.table(fname, header=TRUE, as.is=TRUE) 
		
		lookup <<- new.env()
		
		for(i in 1:nrow(d)){
			lookup[[toString(d$WBIC[i])]] = c(d$LAT[i], d$LON[i])
		}
	}
	lookup[[WBIC]]
}})

#'@title Get perimeter for a given lake
#'@description
#'Get the perimeter of a lake with given ID
#'
#'@param site_id The character ID for the requested data
#'
#'@return
#' Perimeter in meters
#'@details
#'
#'
#'
#'@author 
#'Luke Winslow, Jordan Read
#'
#'@examples
#'
#'
#'@export
getPerim <- local({ lookup=NULL; function(WBIC) {
	if (is.null(lookup)) { 
		cat('Caching perimeter info.\n')
		
		fname <- system.file('supporting_files/wbicAreaPerim.tsv', package=getPackageName())
		d <- read.table(fname, header=TRUE, as.is=TRUE) 
		
		lookup <<- new.env()
		for(i in 1:nrow(d)){
			lookup[[toString(d$WBIC[i])]] = d$PERIMETER[i]
		}
	}
	lookup[[WBIC]]
}})

#'@title Get shoreline development factor for a given lake
#'@description
#'Get the shoreline development factor (SDF) of a lake with given ID
#'
#'@param site_id The character ID for the requested data
#'
#'@return
#' Return a SDF (between 1 and infinity)
#'@details
#' The ratio of a a lake's observed perimeter divided by the perimeter of a 
#' circle with the same area as the lake. Cannot be less than 1. 
#'
#'
#'@author 
#'Luke Winslow, Jordan Read
#'
#'@examples
#'
#'
#'@export
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

#'@title Get coefficient of wind drag for a given lake or given wind sheltering coefficient
#'@description
#'Get coefficient of wind drag for a lake with a given ID or a supplied wind sheltering coefficient
#'
#'@param site_id The character ID for the requested data
#'@param Wstr The wind sheltering coefficient
#'
#'@return
#' Coefficient of wind drag 
#'@details
#'
#'
#'
#'@author 
#'Luke Winslow, Jordan Read
#'
#'@examples
#'
#'
#'@export
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

#'@title Get wind sheltering coefficient for a given lake
#'@description
#'Get the wind sheltering coefficient of a lake with given ID
#'
#'@param site_id The character ID for the requested data
#'@param method The desired calculation method one of c('Markfort', 'Hondzo', )
#'@return
#' The wind sheltering coefficient (between 0 and 1)
#'@details
#'
#'
#'
#'@author 
#'Luke Winslow, Jordan Read
#'
#'@examples
#'
#'
#'@export
getWstr	<-	function(WBIC,method='Markfort',canopy=NULL){

	lkeArea	<-	getArea(WBIC)
  
  if(is.na(lkeArea)){
    return(NA)
  }
	
	if (method=='Markfort'){
		# Markfort et al. 2010
		minWstr	<-	0.0001
		if (is.null(canopy)){
			hc	<-	max(c(getCanopy(WBIC),1))
		} else {
			hc	<-	canopy
		}
		
		if(is.na(hc) | is.null(hc)){
			return(NA)
		}
		
		Xt	<-	50*hc
		
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
		# Markfort et al. 2010
		minWstr	<-	0.0001
		if (is.null(canopy)){
			hc	<-	max(c(getCanopy(WBIC),1))
		} else {
			hc	<-	canopy
		}
		
		Xt	<-	50*hc
		
		perim	<-	getPerim(WBIC)
		shelArea	<-	perim*hc*12.5 # 25% of Markfort, as sheltering is single direction...
		shelter	<-	(lkeArea-shelArea)/lkeArea
		wind.shelter	<-	max(c(shelter,minWstr))
		if (is.null(perim)){wind.shelter<-NULL}
	}
	
	return(wind.shelter)
}

#'@title Get max depth for a given lake
#'@description
#'Get the max depth for a given lake id
#'
#'@param site_id The character ID for the requested data
#'
#'@return
#' Max observed depth in meters
#'@details
#'
#'
#'
#'@author 
#'Luke Winslow, Jordan Read
#'
#'@examples
#'
#'
#'@export
getZmax <- local({ lookup=NULL; function(WBIC) {
  if (is.null(lookup)) { 
    cat('Caching depth info.\n')
    fname <- system.file('supporting_files/managed_lake_info.txt', package=getPackageName())
    d	<-	read.table(fname, header=TRUE, sep='\t', quote="\"")
    
    lookup <<- new.env()
    ft2m  <-  0.3048
    for(i in 1:nrow(d)){
      mean.m <- d$max.depth.ft[i]
      lookup[[toString(d$WBIC[i])]] = mean(ft2m*mean.m)
    }
  }
  lookup[[WBIC]]
}})

#'@title Get mean depth for a given lake
#'@description
#'Get the mean depth for a given lake id
#'
#'@param site_id The character ID for the requested data
#'
#'@return
#' Mean calculated depth in meters
#'@details
#'
#'
#'
#'@author 
#'Luke Winslow, Jordan Read
#'
#'@examples
#'
#'
#'@export
getZmean	<-	function(WBIC){
	ft2m	<-	0.3048
	fname <-  system.file('supporting_files/managed_lake_info.txt', package=getPackageName())
	data	<-	read.table(fname, header=TRUE, sep='\t', quote="\"")
	useI	<-  data$WBIC==as.numeric(WBIC)
	mean.depth	<-	NA
	if (any(useI)){
		mean.depth	<-	ft2m*mean(data$mean.depth.ft[useI],na.rm=TRUE)
	}
	if (is.na(mean.depth)){
		mean.depth	<-	1/3*getZmax(WBIC)
	}
	return(mean.depth)
}

#'@title Get the ice on date for a given lake
#'@description
#'Get the ice on date for a given lake id and year
#'
#'@param site_id The character ID for the requested data
#'@param year The year for which you need the ice-on date
#'@return
#' Max observed depth in meters
#'@details
#'
#'
#'
#'@author 
#'Luke Winslow, Jordan Read
#'
#'@examples
#'
#'
#'@export
getIceOn	<-	function(WBIC,year){
  early.freeze	<-	8	# month of year
	# the ice on for each lake for a given year
	# ice on is assumed to either happen during the same calendar year, or within the NEXT year
  
  fname <- system.file('supporting_files/empirical.ice.tsv', package=getPackageName())
	empir.ice = read.table(fname, sep='\t', header=TRUE, as.is=TRUE) 
	
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

#'@title Get the ice off date for a given lake
#'@description
#'Get the ice off date for a given lake id and year
#'
#'@param site_id The character ID for the requested data
#'@param year The year for which you need the ice-off date
#'@return
#' Max observed depth in meters
#'@details
#'
#'
#'
#'@author 
#'Luke Winslow, Jordan Read
#'
#'@examples
#'
#'
#'@export
getIceOff	<-	function(WBIC,year) {
	# the ice off for each lake for a given year
	# ice off is assumed to happen during the same calendar year

	fname <- system.file('supporting_files/empirical.ice.tsv', package=getPackageName())
	empir.ice = read.table(fname, sep='\t', header=TRUE, as.is=TRUE) 
	
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
