
#'@title Calculate optical thermal habitat using temp and light thresholds 
#'
#'@inheritParams area_light_temp_threshold
#'
#'@return data.frame with three columns. opti_hab, therm_hab, opti_therm_hab 
#'for areas of each habitat type (with opti_therm_hab being the overlap of both)
#'
#'
#'@export
opti_thermal_habitat = function(nc_file, nml_file, irr_thresh=c(0,2000), wtr_thresh=c(0,25), area_type="benthic"){
	
	nml = read_nml(nml_file)
	
	kd = get_nml_value(nml, 'Kw')
	
	bathy = get_hypsography(nml)
	names(bathy) = c('depth', 'area')
	
	wtr = get_temp(nc_file)
	
	io = get_var(nc_file, 'I_0')
	
	light_alone = area_light_threshold(kd, io[,2], irr_thresh, bathy, area_type)
	temp_alone  = area_temp_threshold(wtr, wtr_thresh, bathy, area_type)
	light_temp  = area_light_temp_threshold(wtr, kd, io[,2], irr_thresh, wtr_thresh, bathy, area_type)
	
	return(data.frame(opti_hab=light_alone, therm_hab=temp_alone, opti_therm_hab=light_temp))
}
