
lakes= read.csv('d:/wbics_candidate_study_lakes.csv', header=TRUE, as.is=TRUE)


secchi = c(10:3, seq(2.9,0.1, by=-0.1))

for(j in 1:nrow(lakes)){
	lakeid = as.character(lakes$WBIC[j])
	out = data.frame()
	
	for(i in 1:length(secchi)){
	
		prep_run_chained_glm_kd(lakeid, kd=1.7/secchi[i], path='d:/test')
		
		out = rbind(out, chained.habitat.calc('d:/test', lakeid = lakeid))
		
	}
	
	out$secchi = secchi
	png(paste0(lakes$Lake[j], '.png'), res=300, width=1800, height=1500)
	
	plot(out$secchi, out$optic_thermal_hab/out$lake_benthic_area/out$simulation_length_days, 
			 main=lakes$Lake[j], ylab='Area Fraction', xlab='secchi (m)', pch=20, type='o')
	
	dev.off()
}
