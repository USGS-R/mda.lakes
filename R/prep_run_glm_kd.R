#'@title Prepare and run  model with Kd scenario
#'
#'@param site_id 
#'@param path Directy to start and run model
#'@param kd Directly supplied kd value (if different value getClarity desired)
#'@param years 
#'Vector of numeric years to simulate. Will simulate from Apr 01 of the 
#'first year to March 01 of the last year + 1
#'
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
prep_run_glm_kd <- function(site_id, path, years,
																kd = getClarity(site_id, default.if.null=TRUE), 
																nml_args=NULL, sed_heat=FALSE){
	
	
	nml_obj = populate_base_lake_nml(site_id, kd=kd)
	
	
	#finally, change supplied nml
	if(!is.null(nml_args)){
		nml_obj = set_nml(nml_obj, arg_list=nml_args)
	}
	
	start = paste0(years[1], '-04-01 00:00:00')
	end  = paste0( (years[length(years)] + 1), '-03-01 00:00:00') 
	
	if(!sed_heat){
		nml_obj[['sed_heat']] = NULL  # Drop sediment heating if it is there
	}
	nml_obj = set_nml(nml_obj, 'start', start)
	nml_obj = set_nml(nml_obj, 'stop',  end)
	nml_obj = set_nml(nml_obj, 'out_fn', 'output')
	nml_obj = set_nml(nml_obj, 'Kw', kd)
	
	nml_out_path = file.path(path, "glm2.nml")
	write_nml(nml_obj, nml_out_path)
	out_val = run_glm(path)

	
	return(out_val)
}
