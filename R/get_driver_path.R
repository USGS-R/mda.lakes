
#'@title Return driver file location for given lake
#'
#'@description
#'This functoin returns a path to the driver file for a lake. Retrieve and cache
#'the file from Sciencebase if necessary
#'
#'@param fname Name of the file, generally based on the unique site_id
#'@param driver_name The driver source name, options ('NLDAS', 'CM2.0', 'GENMOM', 'ECHAM5')
#'@param loc_cache Should the locally cached version of the driver file be used if available.
#'@param base_url Root of the URL where driver files are served
#'@param ... Extra parameters are ignored
#'
#'@author Luke Winslow
#'
#'
#'@export
get_driver_path = function(fname, driver_name='NLDAS', loc_cache=TRUE, base_url='http://cida-test.er.usgs.gov/mda.lakes/', ...){
	
	
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
	
	status = 1
	#catch the download file error, and give more descriptive error
	# try three times if status was not success
	for(i in 1:3){
		tryCatch({
			status = download.file(full_url, destfile = dest, quiet=TRUE)
		}, error=function(e){})
		
		if(status == 0){
			break
		}else{
			cat('retrying driver download...\n')
		}
	}
	#loc = unzip(driver_path, files=fname, exdir=dest)
	
	if(status != 0){
		#if download failed, delete file that might have been only partially created
		unlink(dest)
		stop(fname, ':file of that name does not exist for driver:', driver_name)
	}
	
	return(dest)
}


