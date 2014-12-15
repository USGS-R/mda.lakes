#'
#'@title Check met data status and download if finished GDP processing
#'
#'@param gdp_list List of GDP process objects from \code{\link{geoknife}}
#'@param dest_path Destiation directory for GDP output to be saved
#'
#'@return The list with finished entries removed
#'
#'
#'@export
met_check_download_gdp = function(gdp_list, dest_path='.'){

	if(!require(geoknife)){
		stop('Geoknife must be installed for met_drivers functions')
	}
	
	if(length(gdp_list) == 0){
		return(list())
	}
	
	for(i in 1:length(gdp_list)){
		#cat(i,'\n')
		gk = gdp_list[[i]]
		
		if (isSuccessful(gk)){
			cat(names(gdp_list)[i], ' is done. Downloading....\n')
			status.geoknife = checkProcess(gk)
			download.file(status.geoknife$URL, file.path(dest_path, names(gdp_list)[i]))
			gdp_list[[i]] = NULL
			break
		}
		
		if (isError(gk)){
			status.geoknife = checkProcess(gk)
			warning(status.geoknife$status)
			gdp_list[[i]] = NULL
			break
		}
	}
	
	return(gdp_list)
}