# run one WiLMA lake (Mendota) using new model running technique

site_id = 'nhd_13344210'

library(mda.lakes)
library(glmtools)
library(rLakeAnalyzer)

#setwd('d:/test')
simdir = 'd:/test'

prep_run_glm_kd(site_id = site_id, 
					path = simdir,
				  years = 1979:2014,
					kd    = get_kd_avg(site_id)$kd_avg,
					nml_args=list(
						dt=3600, subdaily=FALSE, nsave=24, 
						timezone=-6,
						csv_point_nlevs=0)
					)


##Now combine modeled and calibrated data and output
sims = Sys.glob(file.path(simdir, 'output*.nc'))

depths = get.offsets(get_temp(sims[1], reference = 'surface'))

#loop over years
all.data = data.frame()
for(i in 1:length(sims)){
	tmp = get_temp(sims[i], reference='surface', z_out=depths)
	all.data = rbind(all.data, tmp)
}

write.table(all.data, paste0('', lake_id, '.wtr.tsv'),
						sep='\t', row.names=FALSE)

