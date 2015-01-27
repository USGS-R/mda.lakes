#'@title Prepare and run  model with Kd scenario
#'
#'@param site_id 
#'@param path Directy to start and run model
#'@param kd Directly supplied kd value (if different value getClarity desired)
#'@param nml_args Arguments for nml file to override defaults
#'
#'
#'@description 
#'Pulls in the required information to do a simple GLM 
#'run. Grabs driver data from SB and attribute data
#'from the internal package DB. Uses supplied Kd values.
#'Does \emph{NOT} use ice on/off to break up simulation.
#'
#'@examples
#'dir.create('~/test')
#'	dir.create(to_run[i])
#'	
#'	prep_run_glm(to_run[i], path=to_run[i], 
#'							 start=as.POSIXct('1979-01-01'), 
#'							 end=as.POSIXct('2011-12-31'))
#'									 
#'
#'@import glmtools
#'@import GLMr
#'@export
prep_run_glm_kd <- function(site_id, path, 
																start=as.POSIXct('2008-04-01'), 
																end=as.POSIXct('2008-06-01'), 
																kd = getClarity(site_id, default.if.null=TRUE), 
																nml_args=NULL){
	
	
	nml_obj = populate_base_lake_nml(site_id)
	
	
	#finally, change supplied nml
	if(!is.null(nml_args)){
		nml_obj = set_nml(nml_obj, arg_list=nml_args)
	}
	
	
	nml_obj = set_nml(nml_obj, 'start', format(as.POSIXct(start), '%Y-%m-%d %H:%M:%S'))
	nml_obj = set_nml(nml_obj, 'stop', format(as.POSIXct(end), '%Y-%m-%d %H:%M:%S'))
	nml_obj = set_nml(nml_obj, 'out_fn', 'output')
	nml_obj = set_nml(nml_obj, 'Kw', kd)
	
	nml_out_path = file.path(path, "glm2.nml")
	write_nml(nml_obj, nml_out_path)
	out_val = run_glm(path)

	
	return(out_val)
}
