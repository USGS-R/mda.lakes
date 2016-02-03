get_driver_nhd = function(id, driver_name, loc_cache, timestep){
	
	#get index
	indx = get_driver_index(driver_name, loc_cache)
	
	#match id to index
	match_i = which(indx$id == id)
	if(length(match_i) < 8){
		stop('flawed or missing driver set for ', id)
	}
	
	driver_df  = data.frame()
	driver_env = new.env()
	#grab (and open?) Rdata files
	for(i in 1:length(match_i)){
		fname = indx[match_i[i], 'file.name']
		driver_url = paste0(base_url, 'drivers_GLM_', driver_name, '/', fname)
		dest = file.path(tempdir(), driver_name, fname)
		
		if(!download_helper(driver_url, dest)){
			stop('failure downloading ', fname, '\n')
		}
		
		load(dest, envir=driver_env)
		driver_env[[indx[match_i[i], 'variable']]] = driver_env[['data.site']]
	}
	
	rm('data.site', envir=driver_env)
	
	#create and save formatted dataframe
	all_drivers = Reduce(function(...) merge(..., by='DateTime'), lapply(ls(driver_env), function(x)driver_env[[x]]))
	
	glm_drivers = nldas_to_glm(all_drivers)
	
	if(timestep=='daily'){
		daily = trunc(as.POSIXct(glm_drivers$time), units='days')
		glm_drivers$time = format(daily,'%Y-%m-%d %H:%M:%S')
		
		glm_drivers = plyr::ddply(glm_drivers,'time', function(df){colMeans(df[,-1])})
		
	}
	
	dest = paste0(tempdir(), '/', driver_name, '/', id, '.csv')
	if(!file.exists(dirname(dest))){
		dir.create(dirname(dest), recursive=TRUE)
	}
	
	write.table(glm_drivers, dest, sep=',', row.names=FALSE, col.names=TRUE, quote=FALSE)
	
	return(dest)
}

get_driver_index = function(driver_name, loc_cache){
	#see if index file exists already
	index_url = paste0(base_url, 'drivers_GLM_', driver_name, '/driver_index.tsv')
	dest = file.path(tempdir(), driver_name, 'driver_index.tsv')
	
	#If it exists, return without downloading
	if(file.exists(dest) && loc_cache){
		return(read.table(dest, sep='\t', header=TRUE, as.is=TRUE))
	}
	
	if(!download_helper(index_url, dest)){
		stop('driver_index.tsv: unable to download for driver data:', driver_name)
	}
	
	return(read.table(dest, sep='\t', header=TRUE, as.is=TRUE))
}

nldas_to_glm = function(nldas_data){
	
	## convert and downsample wind
	
	nldas_data$WindSpeed = sqrt(nldas_data$ugrd10m^2 + nldas_data$vgrd10m^2)
	nldas_data$ShortWave = nldas_data$dswrfsfc
	nldas_data$LongWave  = nldas_data$dlwrfsfc
	nldas_data$AirTemp   = nldas_data$tmp2m - 273.15 #convert K to deg C
	nldas_data$Rain      = nldas_data$apcpsfc*24/1000 #convert to m/day rate
	nldas_data$RelHum    = 100*nldas_data$spfh2m/qsat(nldas_data$tmp2m-273.15, nldas_data$pressfc*0.01)
	
	#now deal with snow
	nldas_data$Snow = 0
	
	# 10:1 ratio assuming 1:10 density ratio water weight
	nldas_data$Snow[nldas_data$AirTemp < 0] = nldas_data$Rain[nldas_data$AirTemp < 0]*10 
	nldas_data$Rain[nldas_data$AirTemp < 0] = 0
	
	#convert DateTime to properly formatted string
	nldas_data$time = nldas_data$DateTime
	nldas_data$time = format(nldas_data$time,'%Y-%m-%d %H:%M:%S')
	
	return(nldas_data[order(nldas_data$time), c('time','ShortWave','LongWave','AirTemp','RelHum','WindSpeed','Rain','Snow')])
}
