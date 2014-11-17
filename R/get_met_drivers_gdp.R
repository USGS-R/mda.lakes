#'
#'@title Submit and retrieve GDP processing request for met drivers
#'@param dest_path Folder path to store downloaded files when finished
#'@param service The source data thredds service
#'@param start Start date for met data timeseries as \link{POSIXct}
#'@param end End date for met data timeseries as \link{POSIXct}
#'
#'@description
#' Available services:\cr
#' \code{nldas}\cr
#' \code{regclim:echam5}\cr
#' \code{regclim:genmom}\cr
#' \code{regclim:cm2.0}\cr
#' \code{regclim:ncep}
#' 
#'
#'@author Luke Winslow, Jordan Read
#'
#'@import geoknife
#'
#'@export
get_met_drivers_gdp <- function(dest_path='.', service='regclim:ncep',
																start=as.POSIXct('1990-01-01'), end=as.POSIXct('1990-01-03')){

	
	#precip  apcpsfc.tsv
	#dwLW    dlwrfsfc.tsv
	#dwSW 	 dswrfsfc.tsv
	#press 	 pressfc.tsv
	#spfHum  spfh2m.tsv
	#airT_k  tmp2m.tsv
	#uwnd 	 ugrd10m.tsv
	#vwnd    vgrd10m.tsv
	
	if(tolower(service)=='nldas'){
		
		datasetURI	<-	'dods://hydro1.sci.gsfc.nasa.gov:80/dods/NLDAS_FORA0125_H.002'
		vars	   <- c('apcpsfc','pressfc','vgrd10m','ugrd10m','dlwrfsfc','dswrfsfc','spfh2m','tmp2m')
		out_vars <- c('apcpsfc','pressfc','vgrd10m','ugrd10m','dlwrfsfc','dswrfsfc','spfh2m','tmp2m')
		
	##Currently, all 'regclim:' models are only the eastern region as they were run 
	## region-by-region (not North America wide)
	}else if(tolower(service)=='regclim:echam5'){
		datasetURI = 'dods://regclim.coas.oregonstate.edu:8080/thredds/dodsC/regcmdata/EH5/ena/Daily/RegCM3_Daily_ena_EH5.ncml'
		vars	   <- c('RT',     'PSRF',   'VA',      'UA',      'LWD',     'SWI',     'RHA',    'TA')
		out_vars <- c('apcpsfc','pressfc','vgrd10m','ugrd10m','dlwrfsfc','dswrfsfc','relative_hum','tmp2m')
		
	}else if(tolower(service)=='regclim:genmom'){
		
		datasetURI = 'dods://regclim.coas.oregonstate.edu:8080/thredds/dodsC/regcmdata/GENMOM/ena/Daily/RegCM3_Daily_ena_GENMOM.ncml'
		vars	   <- c('RT',     'PSRF',   'VA',      'UA',      'LWD',     'SWI',     'RHA',    'TA')
		out_vars <- c('apcpsfc','pressfc','vgrd10m','ugrd10m','dlwrfsfc','dswrfsfc','relative_hum','tmp2m')
		
	}else if(tolower(service)=='regclim:cm2.0'){
		
		datasetURI = 'dods://regclim.coas.oregonstate.edu:8080/thredds/dodsC/regcmdata/GFDL/ena/Daily/RegCM3_Daily_ena_GFDL.ncml'
		vars	   <- c('RT',     'PSRF',   'VA',      'UA',      'LWD',     'SWI',     'RHA',    'TA')
		out_vars <- c('apcpsfc','pressfc','vgrd10m','ugrd10m','dlwrfsfc','dswrfsfc','relative_hum','tmp2m')
		
	}else if(tolower(service)=='regclim:ncep'){
		
		datasetURI = 'dods://regclim.coas.oregonstate.edu:8080/thredds/dodsC/regcmdata/NCEP/ena/Daily/RegCM3_Daily_ena_NCEP.ncml'
		vars	   <- c('RT',     'PSRF',   'VA',      'UA',      'LWD',     'SWI',     'RHA',    'TA')
		out_vars <- c('apcpsfc','pressfc','vgrd10m','ugrd10m','dlwrfsfc','dswrfsfc','relative_hum','tmp2m')
		
	}
		
	
  geoknife  <-	geoknife()
  # ---- variables -----
  WFS <-  'https://www.sciencebase.gov/catalogMaps/mapping/ows/54668b7ce4b04d4b7dbda52d'#50d35261e4b062c7914ebd14'#50ed714ae4b0438b00db07e4'
  WPS <- 'http://cida.usgs.gov/gdp/process/WebProcessingService'#'http://cida-test.er.usgs.gov/gdp/process/WebProcessingService'
  
  feature_collection  <-  'sb:managed_wiscoNoZ_wgs84'
  attribute <- 'WBDY_WBIC'
  
	
  setWPS(geoknife)<-WPS
  
  # set processing algorithm to feature weighted grid statistics (unweighted will likely fail, because the ring won't intersect the centroids)
  setAlgorithm(geoknife) <- getAlgorithms(geoknife)[4] # feature weighted
      
	output = list()
		
  for (i in 1:length(vars)){
    # set the post inputs for the processing dataset
    setProcessInputs(geoknife) <- list('DATASET_ID'=vars[i],
                                       'DATASET_URI'=datasetURI,
                                       'TIME_START'=format(start, '%Y-%m-%dT%H:%M:%SZ'),
                                       'TIME_END'=format(end, '%Y-%m-%dT%H:%M:%SZ'),
                                       'DELIMITER'='TAB')
    
    # set the web feature service
    setWFS(geoknife) <- WFS
    
    # set the feature collection for the element in the service that you want to use
    setFeature(geoknife) <- list('FEATURE_COLLECTION'=feature_collection,
                                 'ATTRIBUTE'=attribute)
    

    # execute what you have
    geoknife  <-	startProcess(geoknife)
    Sys.sleep(10)
    
    output[[vars[i]]] = geoknife
    
    # print it out so you know how the geoknife object is defined at this point
		cat(vars[i], ':', geoknife@processID, '\n')
  }
  
  #return(output)
	
	
	#wait and recheck the runs every 10 seconds
	repeat{
		Sys.sleep(10)
		
		for(i in 1:length(output)){
			#cat(i,'\n')
			gk = output[[i]]
			
	    if (isSuccessful(gk)){
	    	cat(names(output)[i], ' is done. Downloading....\n')
	    	status.geoknife = checkProcess(gk)
	    	download.file(status.geoknife$URL, file.path(dest_path, names(output)[i]))
	    	output[[i]] = NULL
	    	break
	    }
			
		  if (isError(gk)){
		  	status.geoknife = checkProcess(gk)
		  	cat(status.geoknife$status, '\n')
		  	output[[i]] = NULL
		  	break
		  }
		}
		
		if(length(output) == 0){
			break
		}
	}
}
