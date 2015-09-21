# run one WiLMA lake (Mendota) using new model running technique



lake_id = 1571800
lake_id = as.character(lake_id)

library(mda.lakes)
library(lakeattributes)
library(glmtools)
library(rLakeAnalyzer)

setwd('d:/test')

secchi = get_kd_best(paste0('WBIC_', lake_id), years=1979:2012)

driver_function = function(site_id){
	nldas = read.csv(get_driver_path(paste0(site_id, '.csv'), driver_name = 'NLDAS'), header=TRUE)
	drivers = driver_nldas_wind_debias(nldas)
	drivers = driver_add_burnin_years(drivers, nyears=2)
	drivers = driver_add_rain(drivers, month=7:9, rain_add=1)
	driver_save(drivers)
}

driver_path = driver_function(paste0('WBIC_', lake_id))
driver_path = gsub('\\\\', '/', driver_path)

prep_run_glm_kd(site_id = lake_id, 
					path = '.',
					years = 1979:2012,
					sed_heat = FALSE,
					kd = 1.7/secchi$secchi_avg,
					nml_args=list(
						dt=3600, subdaily=FALSE, nsave=24,
						timezone=-6,
						snow_albedo_factor=1.1,
						meteo_fl=driver_path,
						csv_point_nlevs=0))


##Now combine modeled and calibrated data and output

test = continuous.habitat.calc('d:/test', lakeid = lake_id)

lake_id