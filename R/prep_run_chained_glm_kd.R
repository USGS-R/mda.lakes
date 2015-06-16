#'@title Prepare and run chained model with Kd scenario
#'
#'@param site_id 
#'@param path Directy to start and run model
#'@param kd Directly supplied kd value or values. Must be length 1 (repeated across all years) or same length as number of years simulated
#'@param years Vector of numeric years to simulate.
#'@param nml_args Arguments for nml file to override defaults
#'
#'
#'@description 
#'Pulls in the required information to do a simple GLM 
#'run. Grabs driver data from SB and attribute data
#'from the internal package DB. Uses supplied Kd values
#'
#'@examples
#'dir.create('~/test')
#'	dir.create(to_run[i])
#'	
#'	prep_run_chained_glm(to_run[i], path=to_run[i], 
#'									 start=as.POSIXct('1979-01-01'), 
#'									 end=as.POSIXct('2011-12-31'))
#'									 
#'
#'@import glmtools
#'@import GLMr
#'@export
prep_run_chained_glm_kd <- function(site_id, path, years, 
																		kd = getClarity(site_id, default.if.null=TRUE), 
																		nml_args=NULL, 
																		verbose=FALSE, 
																		ice_src='empirical.ice.tsv'){
	
	
	nml_obj = populate_base_lake_nml(site_id, kd[1])
	
	
	#finally, change supplied nml
	if(!is.null(nml_args)){
		nml_obj = set_nml(nml_obj, arg_list=nml_args)
	}
	
	#iterate over all years
	out_vals = rep(NA, length(years))
	
	#expand kd if necessary
	if(length(kd) == 1){
		kd = rep(kd, length(years))
	}else if(length(kd) != length(years)){
		stop('length of supplied kd must be one or the number of years')
	}
	
	#delete any nc files in the model path
	unlink(Sys.glob(paste0(path, '/output*.nc')))
	
	for(i in 1:length(years)){
		year = years[i]
		
		off_date = getIceOff(site_id, year, ice_src)
		on_date = getIceOn(site_id, year, ice_src)
		
		if(is.na(off_date) | is.na(on_date)){
			warning(site_id, ' does not have ice ON and OFF dates for ', year, '\nSkipping...')
			next
		}
		
		if(is.na(kd[i])){
			warning('Missing kd value. Skipping ', site_id, ":", year)
			next
		}
		
		#start stop
		nml_obj = set_nml(nml_obj, 'start', format(as.POSIXct(off_date), '%Y-%m-%d %H:%M:%S'))
		nml_obj = set_nml(nml_obj, 'stop', format(as.POSIXct(on_date), '%Y-%m-%d %H:%M:%S'))
		nml_obj = set_nml(nml_obj, 'out_fn', paste0('output', year))
		nml_obj = set_nml(nml_obj, 'Kw', kd[i])
		
		nml_out_path = file.path(path, "glm2.nml")
		write_nml(nml_obj, nml_out_path)
		out_vals[i] = run_glm(path, verbose=verbose)
	}
	
	output = data.frame(year=years, kd=kd, out_val=out_vals)
	
	return(output)
}
