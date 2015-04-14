# run one WiLMA lake (Mendota) using new model running technique

lake_id = 2327500
lake_id = as.character(lake_id)

library(mda.lakes)
library(glmtools)
library(rLakeAnalyzer)

setwd('d:/test')

prep_run_chained_glm_secchi(site_id = lake_id, 
					path = '.',
					start = as.POSIXct('1979-01-01'), 
					end = as.POSIXct('2012-12-30'),
					secchi_trend = 0,
					nml_args=list(
					dt=3600, subdaily=FALSE, nsave=24, 
					timezone=-6,
					csv_point_nlevs=0))


##Now combine modeled and calibrated data and output

test = chained.habitat.calc('d:/test', lakeid = lake_id)

