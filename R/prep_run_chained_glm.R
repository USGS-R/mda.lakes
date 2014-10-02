#'@title Prepare and run chained model based on unique site_id
#'
#'@param site_id
#'@param path 
#'@param nml_args
#'
#'@description 
#'Pulls in the required information to do a simple GLM 
#'run. Grabs driver data from Sciencbase and attribute data
#'from the internal package DB. 
#'
#'@examples
#'dir.create('~/test')
#'prep_run_chained_glm('10000', '~/test')
#'
#'@import glmtools
#'@import GLMr
#'@export
prep_run_chained_glm <- function(site_id, path, start=as.POSIXct('2008-04-01'), 
																 end=as.POSIXct('2008-06-01'), nml_args=NULL){
	
	
	nml_obj = populate_base_lake_nml(site_id)
	
	
	#finally, change supplied nml
	if(!is.null(nml_args)){
		nml_obj = set_nml(nml_obj, arg_list=nml_args)
	}
										
	#Write nml file
	#nml_out_path = file.path(path, "glm.nml")
	#write_nml(nml_obj, nml_out_path)
	
	#iterate over all years
	start_year = as.POSIXlt(start)$year+1900
	end_year = as.POSIXlt(end)$year+1900
	
	for(year in start_year:end_year){
		
		off_date = getIceOff(site_id, year)
		on_date = getIceOn(site_id, year)
		
		if(is.na(off_date) | is.na(on_date)){
			warning(site_id, ' does not have ice ON and OFF dates for ', year, '\nSkipping...')
			next
		}
		
		
		#start stop
		nml_obj = set_nml(nml_obj, 'start', format(as.POSIXct(off_date), '%Y-%m-%d %H:%M:%S'))
		nml_obj = set_nml(nml_obj, 'stop', format(as.POSIXct(on_date), '%Y-%m-%d %H:%M:%S'))
		nml_obj = set_nml(nml_obj, 'out_fn', paste0('output', year))
		
		nml_out_path = file.path(path, "glm.nml")
		write_nml(nml_obj, nml_out_path)
		run_glm(path)
	}
	
}


