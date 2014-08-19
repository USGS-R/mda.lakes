#update.nldas <- function(time.start,time.end,chunk){
  require(geoknife)
  geoknife  <-	geoknife()
  # ---- variables -----
  WFS <-  'https://www.sciencebase.gov/catalogMaps/mapping/ows/50d35261e4b062c7914ebd14'#50ed714ae4b0438b00db07e4'
  WPS <- 'http://cida.usgs.gov/gdp/process/WebProcessingService'#'http://cida-test.er.usgs.gov/gdp/process/WebProcessingService'
  
  feature_collection  <-  'sb:managedLakesAllOne'
  attribute <- 'WBDY_WBIC'
  datasetURI	<-	'dods://hydro1.sci.gsfc.nasa.gov:80/dods/NLDAS_FORA0125_H.002'
  vars	<-	 c('apcpsfc','pressfc','vgrd10m','ugrd10m','dlwrfsfc','dswrfsfc','spfh2m','tmp2m')
  # ---- variables -----
  
  setWPS(geoknife)<-WPS
  
  # set processing algorithm to feature weighted grid statistics (unweighted will likely fail, because the ring won't intersect the centroids)
  setAlgorithm(geoknife) <- getAlgorithms(geoknife)[4] # feature weighted
      
  for (i in 1:length(vars)){
    # set the post inputs for the processing dataset
    setProcessInputs(geoknife) <- list('DATASET_ID'=vars[i],
                                       'DATASET_URI'=datasetURI,
                                       'TIME_START'='2013-01-01T00:00:00Z',
                                       'TIME_END'='2013-12-31T23:00:00Z',
                                       'DELIMITER'='TAB')
    
    # set the web feature service
    setWFS(geoknife) <- WFS
    
    # set the feature collection for the element in the service that you want to use
    setFeature(geoknife) <- list('FEATURE_COLLECTION'=feature_collection,
                                 'ATTRIBUTE'=attribute)
    

    # execute what you have
    geoknife  <-	startProcess(geoknife)
    Sys.sleep(10)
    
    # print it out so you know how the geoknife object is defined at this point
	cat(vars[i]); cat(': ')
    cat(geoknife@processID); cat('\n')
    
  }
  
  
  # execute what you have
  geoknife	<-	startProcess(geoknife)
  
  status.geoknife  <-  checkProcess(geoknife)
  
  cat('checking status of GDP request. Large complex requests take longer to process.\n')
  repeat{
    if (!is.null(status.geoknife$URL) | status.geoknife$status!=""){
      break
    }
    cat('checking process...\n')
    Sys.sleep(10)
    if (is.null(status.geoknife$URL)){
      status.geoknife  <-  checkProcess(geoknife)
    }
  }
  
  if (status.geoknife$status=='Process successful'){
    cat(paste(status.geoknife$status,'\nDownload available at: ',status.geoknife$URL,sep=''))
  } else {
    cat(status.geoknife$status)
  }
  
#}