#' @title Download helper to handle retries
#' 
#' @description Attempts to download file to destination, retries if
#' the download fails for whatever reason
#' 
#' @param url Full file URL
#' @param dest Desired file destination
#' @param retries Number of download retries in case of failure (default:3)
#' 
#' @return Returns TRUE if download successful, 1 if download fails
#' 
#' 
download_helper = function(url, dest, retries=3){
	
	if(retries < 1){
		warning('retries must be 1 or higher, setting to 1')
		retries = 1
	}
	
	if(!file.exists(dirname(dest))){
		warning('Creating destination directory')
		dir.create(dirname(dest), recursive=TRUE)
	}
	
	status = 1
	#catch the download file error, and give more descriptive error
	# try three times if status was not success
	for(i in 1:retries){
		tryCatch({
			status = download.file(url, destfile = dest, quiet=TRUE)
		}, error=function(e){})
		
		if(status == 0){
			break
		}else{
			cat('retrying', basename(url), 'download...\n')
		}
	}
	#loc = unzip(driver_path, files=fname, exdir=dest)
	
	if(status != 0){
		#if download failed, delete file that might have been only partially created
		unlink(dest)
		
	}
	return(status == 0)
}