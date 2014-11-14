# run one WiLMA lake based on command line input

#Setup libraries
dir.create('rLibs')

install.packages('mda.lakes', repos='file:packages', lib='rLibs')

args <- commandArgs(trailingOnly = TRUE)

cat(args, '\n')

lake_indx = as.numeric(args[1])+1

lake_ids = read.table('lake_ids.tsv', sep='\t', header=TRUE)
lake_ids = as.character(lake_ids$WBIC)

cat(lake_ids[lake_indx], '\n')

library(mda.lakes)
library(glmtools)

prep_run_chained_glm_secchi(site_id = lake_ids[lake_indx], 
										 path = '.',
										 start = as.POSIXct('1979-01-01'), 
										 end = as.POSIXct('2012-12-30'),
										 secchi_trend = 0,
										 nml_args=list(
										 	dt=3600, subdaily=FALSE, nsave=24, 
										  timezone=-6,
										  csv_point_nlevs=0))

#chained.habitat.calc.kevin('.', lakeid=lake_ids[lake_indx], 
#		output.path = paste0('kevin.', lake_ids[lake_indx], '.output.tsv'))


##Now combine modeled and calibrated data and output
sims = Sys.glob('output*.nc')

obs = read.table(system.file('supporting_files/wtemp.obs.tsv', package = 'mda.lakes'), 
					 sep='\t', header=TRUE, as.is=TRUE)
obs = obs[obs$WBIC == lake_ids[lake_indx], c('DATETIME', 'DEPTH', 'WTEMP')]
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

write.table(cal.data, paste0('cal.', lake_ids[lake_indx], '.tsv'), 
						sep='\t', row.names=FALSE)

unlink('obs.tsv')

