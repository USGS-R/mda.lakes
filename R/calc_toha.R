

#'@title calculate the Thermal Optical Habitat Area (TOHA) for a given lake
#'
#'@param lakeid ID of lake to run
#'@param secchi Vector of secchi values to run model over
#'@param wtr_thresh The temperature thresholds in degC (defaults from Lester et al 2004)
#'@param irr_thresh The light intensity thresholds in W/m^2 (defaults from Lester et al 2004)
#'
#'@description
#'This runs GLM using a series of different secchi values, and for each calculates the TOHA.
#'
#'
#'@export
calc_toha = function(lakeid, secchi=c(seq(0.1,2.9, by=0.1),3:10), wtr_thresh=c(11,25), irr_thresh=c(0.0762, 0.6476)){
	
	runpath = tempdir()
	
	hypso = interp_hypso(getBathy(lakeid))
	out = data.frame()
	
	for(i in 1:length(secchi)){
		
		runinfo = prep_run_chained_glm_kd(lakeid, kd=1.7/secchi[i], path=runpath)
		
		ncfile  = paste0(runpath, '/output', runinfo$year, '.nc')
		nmlfile = file.path(runpath, 'glm2.nml')
		
		toha = opti_thermal_habitat(nc_file=ncfile, nml_file=nmlfile, irr_thresh = c(0.0762, 0.6476), 
															 wtr_thresh=c(11,25), interp_hour=TRUE, area_type="benthic")
		out = rbind(out, toha)
	}
	
	out$secchi = secchi
	out$lake_benthic_area = sum(benthic_areas(hypso$depth, hypso$area))
	return(out)
}