#'@title Return driver file location for given lake
#'
#'@description
#'This functoin returns a path to the driver file for a lake. Retrieve and cache
#'the file from Sciencebase if necessary
#'
#'@param id Site id, prepended with id type followed by _ (e.g., 'nhd_' or 'wbic_')
#'@param driver_name The driver source name, options ('NLDAS', 'CM2.0', 'GENMOM', 'ECHAM5')
#'@param loc_cache Should the locally cached version of the driver file be used if available.
#'@param timestep Requested timestep, "hourly" or "daily" (default) accepted
#'@param ... Extra parameters are ignored
#'
#'@author Luke Winslow
#'
#'@examples
#'get_driver_path('nhd_120052892')
#'get_driver_path('nhd_120052892', 'ECHAM5')
#'get_driver_path('nhd_120052892', 'CM2.0')
#'
#'
#'get_driver_path('WBIC_1881900')
#'
#'@export
get_driver_path = function(id, driver_name='NLDAS', loc_cache=TRUE, timestep='daily', ...){
	
	#check for NHD or WBIC
	id_type = strsplit(id, '_')
	if(length(id_type[[1]]) < 2){
		stop('id must have form idtype_id, e.g., nhd_10595606')
	}
	
	if(tolower(id_type[[1]][1]) == 'nhd'){
		return(get_driver_nhd(id, driver_name, loc_cache, timestep=timestep))
		
	}else if(tolower(id_type[[1]][1]) == 'wbic'){
		return(get_driver_wbic(id, driver_name, loc_cache, timestep=timestep))
		
	}else{
		stop('ID Type:', id_type[[1]][1], ' not recognised')
	}
}

#' Internal function for WBIC drivers
#' 
#' @description Old style download for legacy WBIC 
#' driver files
#' 
#' @keywords internal
#' 
get_driver_wbic = function(fname, driver_name, loc_cache, timestep){
	
	#if just site_id is provided, add .csv (some legacy stuff here!)
	if(substr(fname, nchar(fname)-3, nchar(fname)) != '.csv'){
		fname = paste0(fname, '.csv')
	}
	
	full_url = paste0(base_url, 'drivers_GLM_', driver_name, '/', fname)
	dest = file.path(tempdir(), driver_name, fname)
	
	if(file.exists(dest) && loc_cache){
		return(dest)
	} 
	
	if(!file.exists(dirname(dest))){
		dir.create(dirname(dest), recursive=TRUE)
	}
	
	if(!download_helper(full_url, dest)){
		stop(fname, ':file of that name does not exist for driver:', driver_name)
	}
	
	if(timestep=='hourly'){
		warning('WBIC drivers not available as hourly')
	}
	
	return(dest)
}



