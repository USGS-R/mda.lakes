
#'@title Prepare and run model based on unique site_id
#'
#'@description 
#'Pulls in the required information to do a simple GLM 
#'run. Grabs driver data from Sciencbase and 
#'
#'@examples
#'dir.create('~/test')
#'prep_run_chained_glm('10000', '~/test')
#'
#'@export
prep_run_glm <- function(site_id, path, start=as.POSIXct('2008-04-01'), 
												 end=as.POSIXct('2008-06-01'), nml_args=NULL){
	
	nml_obj = populate_base_lake_nml(site_id)
	
	#start stop
	nml_obj = set_nml(nml_obj, 'start', format(start, '%Y-%m-%d %H:%M:%S'))
	nml_obj = set_nml(nml_obj, 'stop', format(start, '%Y-%m-%d %H:%M:%S'))
	
	#finally, change supplied nml
	nml_obj = set_nml(nml_obj, arg_list=nml_args)
	
	#Write nml file
	nml_out_path = file.path(path, "glm2.nml")
	write_nml(nml_obj, nml_out_path)
	
	## Run 
	run_glm(path)
	
}