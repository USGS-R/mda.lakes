# run one WiLMA lake (Mendota) using new model running technique


lake_id = 2897100
lake_id = as.character(lake_id)

library(mda.lakes)
library(lakeattributes)
library(glmtools)
library(rLakeAnalyzer)

run_dir = tempdir()
years = 2020:2097

secchi = get_kd_best(paste0('WBIC_', lake_id), years=years)

driver_function = function(site_id, gcm='ECHAM5'){
	drivers = read.csv(get_driver_path(paste0(site_id, '.csv'), driver_name = gcm), header=TRUE)
	nldas   = read.csv(get_driver_path(paste0(site_id, '.csv'), driver_name = 'NLDAS'), header=TRUE)
	drivers = driver_nldas_debias_airt_sw(drivers, nldas)
	drivers = driver_add_burnin_years(drivers, nyears=2)
	drivers = driver_add_rain(drivers, month=7:9, rain_add=0.5) ##keep the lakes topped off
	driver_save(drivers)
}

driver_path = driver_function(paste0('WBIC_', lake_id))
driver_path = gsub('\\\\', '/', driver_path)

prep_run_glm_kd(site_id = lake_id, 
					path = run_dir,
					years = years,
					sed_heat = FALSE,
					kd = 1.7/secchi$secchi_avg,
					nml_args=list(
						dt=3600, subdaily=FALSE, nsave=24,
						timezone=-6,
						snow_albedo_factor=1.1,
						meteo_fl=driver_path,
						csv_point_nlevs=0))


##Now combine modeled and calibrated data and output

test = continuous.habitat.calc(run_dir, lakeid = lake_id)
test = subset(test, year %in% 1979:2012)

lake_id