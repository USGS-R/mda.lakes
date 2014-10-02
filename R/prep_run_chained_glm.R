#'@title Prepare and run chained model based on unique site_id
#'
#'@description 
#'Pulls in the required information to do a simple GLM 
#'run. Grabs driver data from Sciencbase and attribute data
#'from the internal package DB. 
#'
#'@import glmtools
#'@import GLMr
#'
#'@export
prep_run_chained_glm <- function(site_id, path){
	
	## Construct NML
	#get default template
	nml_obj = read_nml(nml_template_path())
	
	#Run Name
	nml_obj = set_nml(nml_obj, 'lake_name', site_id)
	#elevation
	elev = getElevation(site_id)
	
	#nml_obj = set_nml(nml_obj, 'outl_elvs', elev)
	
	#hypsometry
	hypso = getBathy(site_id)
	
	#sort by increasing height
	hypso$height = elev-hypso$depth
	hypso = hypso[order(hypso$height),]
	
	#write to NML
	nml_obj = set_nml(nml_obj, 'H', hypso$height)
	nml_obj = set_nml(nml_obj, 'A', hypso$area/1000)
	
	#min_layer_thick & max_
	
	#clarity
	nml_obj = set_nml(nml_obj, 'Kw', getClarity(site_id))
	
	
	#lake basin
	area = getArea(site_id)
	bsn_len = sqrt(area/pi)*2;
	nml_obj = set_nml(nml_obj, 'bsn_wid', bsn_len)
	nml_obj = set_nml(nml_obj, 'bsn_len', bsn_len)
										
	#lake lat/lon
	latlon = getLatLon(site_id)
	nml_obj = set_nml(nml_obj, 'latitude', latlon[1])
	nml_obj = set_nml(nml_obj, 'longitude', latlon[2])
	
	#wind sheltering
	nml_obj = set_nml(nml_obj, 'wind_factor', getWstr(site_id, method='Markfort'))
	
	## Pull in driver data
	driver_path = get_driver_path(paste0('WBIC_', site_id, '.csv'))
	
	nml_obj = set_nml(nml_obj, 'meteo_fl', gsub('\\\\', '/', driver_path))
	
	#start stop
	nml_obj = set_nml(nml_obj, 'start', '2008-04-01 00:00:00')
	nml_obj = set_nml(nml_obj, 'stop', '2008-05-01 00:00:00')
	
	#Write nml file
	nml_out_path = file.path(path, "glm.nml")
	write_nml(nml_obj, nml_out_path)
	
	
	## Run 
	run_glm(path)
	
}


