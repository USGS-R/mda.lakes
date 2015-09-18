
#'@title Debias NLDAS wind
#'
#'
#'@description Debias NLDAS wind based on known post-2001 NLDAS/NARR 
#'issue
#'
#'@param drivers A GLM drivers data.frame
#'
#'
#'@export
driver_nldas_wind_debias = function(drivers, ...){
  
  #nldas = read.csv(fpath, header=TRUE)
  drivers$time = as.POSIXct(drivers$time)
  
  after_2001 = drivers$time > as.POSIXct('2001-12-31')
  
  drivers$WindSpeed[after_2001] = drivers$WindSpeed[after_2001] * 0.921
  
  #driver_path = tempfile(fileext='.csv')
  #write.csv(nldas, driver_path, row.names=FALSE, quote=FALSE)
  return(drivers)
}

#'@title Duplicate n years of initial data
#'
#'
#'@export
driver_add_burnin_years = function(drivers, nyears=2){
  #drivers = read.csv(get_driver_path(fname, driver_name), header=TRUE)
  drivers$time = as.POSIXct(drivers$time)
  
  for(i in 1:nyears){
    to_dup = drivers[1:365, ]
    to_dup$time = to_dup$time - as.difftime(365, units='days')  #drop 365 days from the date/time col
    drivers = rbind(to_dup, drivers)
  }
  
  #new_fpath = tempfile(fileext='.csv')
  #write.table(drivers, new_fpath, quote=FALSE, row.names=FALSE, col.names=TRUE, sep=',')
  return(drivers)
}

#'@title Debias a driver dataset using NLDAS
#'
#'@description Debiases airT and SW based on a linear model of driver to NLDAS
#'airT and a simple offset with SW
#'
#'
#'@export
driver_nldas_debias_airt_sw = function(drivers, nldas){
  
  if(is.character(nldas)){
    nldas = read.csv(nldas, header=TRUE)
  }
  
  dscale = drivers #read.csv(fpath, header=TRUE)
  #nldas = read.csv(get_driver_path(paste0(site_id, '.csv'), driver_name='NLDAS'), header=TRUE)
  
  dbiased = dscale
  
  names(nldas) = paste0('nldas_', names(nldas))
  names(nldas)[1] = 'time'
  
  overlap = merge(nldas, dscale, by='time')
  
  #Debias wind with a multiplier
  wnd_multip = 1/(mean(overlap$WindSpeed)/mean(overlap$nldas_WindSpeed))
  dbiased$WindSpeed = dbiased$WindSpeed*wnd_multip
  
  
  #debias airT with linear model
  air_lm = lm(nldas_AirTemp~AirTemp, overlap)
  
  dbiased$AirTemp = predict(air_lm, dbiased)
  
  dbiased$ShortWave = dbiased$ShortWave + (mean(overlap$nldas_ShortWave) - mean(overlap$ShortWave))
  
  #driver_path = tempfile(fileext='.csv')
  
  #write.csv(dbiased, driver_path, row.names=FALSE, quote=FALSE)
  return(dbiased)
}

#'@title Save a driver data.frame to a temporary file
#'
#'@return path to saved file
#'
#'@export
driver_save = function(drivers){
  driver_path = tempfile(fileext='.csv')
  write.csv(drivers, driver_path, row.names=FALSE, quote=FALSE)
  return(driver_path)
}
