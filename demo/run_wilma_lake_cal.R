# run one WiLMA lake (Mendota) using new model running technique

wbic = 977600
lake_id = as.character(wbic)

library(mda.lakes)
library(glmtools)

prep_run_chained_glm_secchi(site_id = lake_id, 
					path = 'd:/test',
					start = as.POSIXct('1990-01-01'), 
					end = as.POSIXct('2012-12-30'),
					secchi_trend = 0,
					nml_args=list(
					dt=3600, subdaily=FALSE, nsave=24, 
					timezone=-6,
					csv_point_nlevs=0))


##Now combine modeled and calibrated data and output
sims = Sys.glob('output*.nc')

plot_temp(sims[29])


obs = read.table(system.file('supporting_files/wtemp.obs.tsv', package = 'mda.lakes'), 
					 sep='\t', header=TRUE, as.is=TRUE)
obs = obs[obs$WBIC == lake_id, c('DATETIME', 'DEPTH', 'WTEMP')]
write.table(obs, 'obs.tsv', sep='\t', row.names=FALSE)


#loop over years
cal.data = data.frame()
for(i in 1:length(sims)){
	tmp = NULL
	tryCatch({tmp = resample_to_field(sims[i], 'obs.tsv')},
					 error=function(e){})
	if(!is.null(tmp)){
		cal.data = rbind(cal.data, tmp)
	}
}

write.table(cal.data, paste0('cal.', lake_id, '.tsv'), 
						sep='\t', row.names=FALSE)

unlink('obs.tsv')

