# run one WiLMA lake (Mendota) using new model running technique

lake_id = 2152800 
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
sims = Sys.glob('output*.nc')

depths = get.offsets(get_temp(sims[1], reference = 'surface'))

#loop over years
all.data = data.frame()
for(i in 1:length(sims)){
	tmp = get_temp(sims[i], reference='surface', z_out=depths)
	all.data = rbind(all.data, tmp)
}

write.table(all.data, paste0('', lake_id, '.wtr.tsv'),
						sep='\t', row.names=FALSE)

