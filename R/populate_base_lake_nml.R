
#'@title Create basic NML file object for a lake
#'
#'@description
#'This creates an NML object and pre-fills it with 
#'key parameters for the site_id
#'
#'@param site_id Unique lake ID
#'
#'
#'@import glmtools
#'@import GLMr
#'@export
populate_base_lake_nml = function(site_id){
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
	nml_obj = set_nml(nml_obj, 'bsn_vals', nrow(hypso))
	
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
	nml_obj = set_nml(nml_obj, 'coef_wind_drag', 
										getCD(Wstr=getWstr(site_id, method='Markfort')))
	
	
	## Pull in driver data
	driver_path = get_driver_path(paste0('WBIC_', site_id, '.csv'))
	
	nml_obj = set_nml(nml_obj, 'meteo_fl', gsub('\\\\', '/', driver_path))
	
	return(nml_obj)
}