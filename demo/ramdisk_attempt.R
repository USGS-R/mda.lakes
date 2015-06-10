
library(lakeattributes)
library(mda.lakes)
library(foreach)
library(dplyr)
library(glmtools)

obs = read.table(system.file('supporting_files/wtemp.obs.tsv', package = 'mda.lakes'), 
								 sep='\t', header=TRUE, as.is=TRUE, , colClasses=c(WBIC='character'))
obs$site_id = paste0('WBIC_', obs$WBIC)

secchi_obs = filter(secchi, source=='in-situ')

#currently using in-situ secchi data, switch to all?
#best_clarity = intersect(secchi_obs$site_id, turbidity$site_id)
to_run = intersect(obs$site_id, secchi_obs$site_id)

results = data.frame(secchi_conf=seq(1.5, 2.2, by=0.1), sat_rmse=NA, sec_rmse=NA)
for(j in 1:nrow(results)){
	
secchi_conv = results$secchi_conf[j]


insitu = foreach(site_id = to_run, .combine=rbind) %do%{
	
	tryCatch({
		get_driver_path(paste0(site_id, '.csv'))
	
	run_dir = paste0('E:/', site_id)
	dir.create(run_dir)
	
	bare_wbic = substr(site_id, 6, nchar(site_id))
	
	lake_obs = obs[obs$WBIC == bare_wbic, c('DATETIME', 'DEPTH', 'WTEMP')]
	write.table(lake_obs, file.path(run_dir, 'obs.tsv'), sep='\t', row.names=FALSE)
	
	kds = get_kd_year(site_id, src='in-situ')
	kds = group_by(kds, year) %>% summarise(secchi_m = mean(secchi_m)) %>%
		filter(year >= 1979, year <= 2012)
	
	prep_run_chained_glm_kd(site_id=bare_wbic, 
													path=run_dir, 
													years=kds$year, 
													kd=secchi_conv/kds$secchi_m, 
													nml_args=list(
														dt=3600, subdaily=FALSE, nsave=24, 
														timezone=-6,
														csv_point_nlevs=0))
	
	sims = Sys.glob(file.path(run_dir,'output*.nc'))
	
	#loop over years
	cal.data = data.frame()
	for(i in 1:length(sims)){
		tmp = NULL
		tryCatch({
			tmp = resample_to_field(sims[i], file.path(run_dir,'obs.tsv'))
		}, error=function(e){})
		if(!is.null(tmp)){
			cal.data = rbind(cal.data, tmp)
		}
	}
	unlink(run_dir, recursive=TRUE)
	
	cal.data
	},
	error = function(e){}, warning=function(w){})
}




satellite = foreach(site_id = to_run, .combine=rbind) %do%{
	
	tryCatch({
		get_driver_path(paste0(site_id, '.csv'))
		
		run_dir = paste0('E:/', site_id)
		dir.create(run_dir)
		
		bare_wbic = substr(site_id, 6, nchar(site_id))
		
		lake_obs = obs[obs$WBIC == bare_wbic, c('DATETIME', 'DEPTH', 'WTEMP')]
		write.table(lake_obs, file.path(run_dir, 'obs.tsv'), sep='\t', row.names=FALSE)
		
		kds = get_kd_year(site_id, src='in-situ')
		kds = group_by(kds, year) %>% summarise(secchi_m = mean(secchi_m)) %>%
			filter(year >= 1979, year <= 2012)
		
		mean_secchi = get_kd_avg(site_id, src='satellite')
		
		prep_run_chained_glm_kd(site_id=bare_wbic, 
														path=run_dir, 
														years=kds$year, 
														kd=secchi_conv/mean_secchi$kd_avg, 
														nml_args=list(
															dt=3600, subdaily=FALSE, nsave=24, 
															timezone=-6,
															csv_point_nlevs=0))
		
		sims = Sys.glob(file.path(run_dir,'output*.nc'))
		
		#loop over years
		cal.data = data.frame()
		for(i in 1:length(sims)){
			tmp = NULL
			tryCatch({
				tmp = resample_to_field(sims[i], file.path(run_dir,'obs.tsv'))
			}, error=function(e){})
			if(!is.null(tmp)){
				cal.data = rbind(cal.data, tmp)
			}
		}
		unlink(run_dir, recursive=TRUE)
		
		cal.data
	},
	error = function(e){}, warning=function(w){})
}

results$sat_rmse[j] = sqrt(mean((satellite$Observed_wTemp - satellite$Modeled_wTemp)^2, na.rm=TRUE))
results$sec_rmse[j] = sqrt(mean((insitu$Observed_wTemp - insitu$Modeled_wTemp)^2, na.rm=TRUE))

}

