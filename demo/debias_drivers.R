
#Debias the data
debias_drivers = function(nldas_path, dscale_path, dbiased_path, shortwave=FALSE){
  
  dscale = read.csv(dscale_path, header=TRUE)
  nldas = read.csv(nldas_path, header=TRUE)
  
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
  
  
  #lets try shortwave if requested
  if(shortwave){
    dbiased$ShortWave = dbiased$ShortWave + (mean(overlap$nldas_ShortWave) - mean(overlap$ShortWave))
  }
  
  if(missing(dbiased_path)){
    return(dbiased)
  }else{
    write.csv(dbiased, dbiased_path, row.names=FALSE, quote=FALSE)
  }
}


