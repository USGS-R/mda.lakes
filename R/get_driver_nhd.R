# Functions for NHD linked drivers NLDAS and Hostetler
#
#
#

get_driver_nhd = function(id, driver_name, loc_cache, timestep){
	
	hostetler_names = c('ECHAM5', 'CM2.0', 'GENMOM')
	
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
	
	all_drivers = na.omit(all_drivers)
	
	glm_drivers = drivers_to_glm(all_drivers)

	
	
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

drivers_to_glm = function(driver_df){
	
	## convert and downsample wind
	
	driver_df$WindSpeed = sqrt(driver_df$ugrd10m^2 + driver_df$vgrd10m^2)
	driver_df$ShortWave = driver_df$dswrfsfc
	driver_df$LongWave  = driver_df$dlwrfsfc
	driver_df$Rain      = driver_df$apcpsfc*24/1000 #convert to m/day rate
	
	if('tmp2m' %in% names(driver_df)){
		driver_df$AirTemp   = driver_df$tmp2m - 273.15 #convert K to deg C
	}else if('airtemp' %in% names(driver_df)){
		driver_df$AirTemp   = driver_df$airtemp #no conversion neede
	}else{
		stop('Unable to find temperature data.\nDriver service must have temp data (named tmp2m or airtemp). ')
	}
	
	if('relhum' %in% names(driver_df)){
		driver_df$RelHum    = 100*driver_df$relhum
	}else if('spfh2m' %in% names(driver_df)){
		driver_df$RelHum    = 100*driver_df$spfh2m/qsat(driver_df$tmp2m-273.15, driver_df$pressfc*0.01)
	}else{
		stop('Unable to find humidity data.\nDriver service must have humidity data (named relhum or spfh2m). ')
	}
	
	
	#now deal with snow
	driver_df$Snow = 0
	
	# 10:1 ratio assuming 1:10 density ratio water weight
	driver_df$Snow[driver_df$AirTemp < 0] = driver_df$Rain[driver_df$AirTemp < 0]*10 
	driver_df$Rain[driver_df$AirTemp < 0] = 0
	
	#convert DateTime to properly formatted string
	driver_df$time = driver_df$DateTime
	driver_df$time = format(driver_df$time,'%Y-%m-%d %H:%M:%S')
	
	return(driver_df[order(driver_df$time), c('time','ShortWave','LongWave','AirTemp','RelHum','WindSpeed','Rain','Snow')])
}

