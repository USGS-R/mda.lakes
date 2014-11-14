
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
	
	initZ = c(0,0.2,getZmax(site_id));
	initT = c(3,4,4);
	initS = c(0,0,0);
	
	defaults = list(
	'max_layers'=1000,
	'min_layer_vol'=0.5,
	'min_layer_thick'=0.2,
	'max_layer_thick'=1,
	'Kw'=0.63,
	#'coef_inf_entrain'=0,
	'coef_mix_conv'=0.23,
	'coef_wind_stir'=0.23,
	'coef_mix_shear'=0.20,
	'coef_mix_turb'=0.51,
	'coef_mix_KH'=0.30,
	'lake_name'=site_id,
	'latitude'=43,
	'longitude'=-89,
	'subdaily'=FALSE,
	'dt'=86400,
	'num_depths'=length(initZ),                  # number of elements in initZ
	'lake_depth'=getZmax(site_id),
	'the_depths'=initZ,
	'the_temps'=initT,
	'the_sals'=initS,
	'wind_factor'=1.0,
	'ce'=0.0013,
	'ch'=0.0014,
	'cd'=0.0013,
	'rain_sw'=FALSE,
	'snow_sw'=TRUE,
	'num_inflows'=0,
	'outl_elvs'=1,
	'outflow_fl'='outflow.csv',
	'num_outlet'=0,
	'bsn_len_outl'=5,
	'bsn_wid_outl'=5)
	
	nml_obj = set_nml(nml_obj, arg_list=defaults)
	
	
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
	nml_obj = set_nml(nml_obj, 'cd', 
										getCD(Wstr=getWstr(site_id, method='Markfort')))
	
  #Max layer thickness
	max_z = getZmax(site_id)
  max_layer = 1
  if(max_z >= 20){
    max_layer = 1.5
  }else if(max_z >= 8 & max_z < 20){
    max_layer = 1
  }else if(max_z >= 5 & max_z < 8){
    max_layer = 0.8
  }else if(max_z >=3 & max_z < 5){
    max_layer = 0.5
  }else{
    max_layer = 0.3
  }
	nml_obj = set_nml(nml_obj, 'max_layer_thick', max_layer)
    
	
	## Pull in driver data
	driver_path = get_driver_path(paste0('WBIC_', site_id, '.csv'))
	
  
	nml_obj = set_nml(nml_obj, 'meteo_fl', gsub('\\\\', '/', driver_path))
	
	return(nml_obj)
}