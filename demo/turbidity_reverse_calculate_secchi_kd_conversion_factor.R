library(lakeattributes)
library(mda.lakes)
library(dplyr)

#secchi and turbidity
insitu_secchi = filter(secchi, source=='in-situ')
secchi_bare = substr(insitu_secchi$site_id, 6, nchar(insitu_secchi$site_id))
turb_bare = substr(turbidity$site_id, 6, nchar(turbidity$site_id))

#managed lakes
fname = system.file('supporting_files/managed_lake_info.txt', package='mda.lakes')
managed_lakes = read.table(fname, header=TRUE, sep='\t', quote="\"", colClasses=c(WBIC='character'))

#wtemp_obs
obs = read.table(system.file('supporting_files/wtemp.obs.tsv', package = 'mda.lakes'), 
								 sep='\t', header=TRUE, as.is=TRUE, colClasses=c(WBIC='character'))


intersect(managed_lakes$WBIC, secchi_bare)

intersect(intersect(intersect(obs$WBIC, secchi_bare), managed_lakes$WBIC), turb_bare)


secc = get_kd_avg(paste0('WBIC_',managed_lakes$WBIC), src = 'in-situ')

secc_turb = merge(secc, get_turbidity_avg(paste0('WBIC_',managed_lakes$WBIC)))
secc_turb = secc_turb[complete.cases(secc_turb), ]
secc_turb$secchi_m = 1.7/secc_turb$kd_avg

conv = secc_turb$secchi_m*(0.29+0.09*secc_turb$turbidity_avg*secc_turb$secchi_m)
