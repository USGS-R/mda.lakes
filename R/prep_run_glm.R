
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
prep_run_glm <- function(site_id, path){
	
	nml_obj = populate_base_lake_nml(site_id)
	
	#start stop
	nml_obj = set_nml(nml_obj, 'start', '2008-04-01 00:00:00')
	nml_obj = set_nml(nml_obj, 'stop', '2008-05-01 00:00:00')
	
	#Write nml file
	nml_out_path = file.path(path, "glm.nml")
	write_nml(nml_obj, nml_out_path)
	
	
	## Run 
	run_glm(path)
	
}