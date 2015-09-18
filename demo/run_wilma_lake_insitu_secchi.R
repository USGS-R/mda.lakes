# run one WiLMA lake (Mendota) using new model running technique

lake_id = 400
lake_id = as.character(lake_id)

library(mda.lakes)
library(glmtools)
library(rLakeAnalyzer)
library(lakeattributes)
library(plyr)

setwd('d:/test')


secchi = get_kd_year(paste0('wbic_', lake_id), src = 'in-situ')

if(nrow(secchi) == 1 & is.na(secchi$kd)){
	stop('no in-situ secchi data for WBIC_', lake_id)
}

year_avg = ddply(secchi, 'year', function(df){mean(df$secchi_m)})

year_avg = year_avg[year_avg$year <=2012 & year_avg$year >= 1979, ]

prep_run_chained_glm_kd(site_id = lake_id, 
														path = '.',
														years = year_avg$year,
														kd = 1.7/year_avg$V1,
														nml_args=list(
															dt=3600, subdaily=FALSE, nsave=24, 
															timezone=-6,
															csv_point_nlevs=0))


##Now combine modeled and calibrated data and output

test = chained.habitat.calc('d:/test', lakeid = lake_id)
