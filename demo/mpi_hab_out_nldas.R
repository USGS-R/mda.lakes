## Lets MPI cluserify things

#first load some modules:
# module load mpi/openmpi-1.5.5-gcc
# module load tools/netcdf-4.3.2-gnu


##install packages locally (available on cluster)
#install.packages(c('devtools', 'rLakeAnalyzer', 'dplyr', 'lubridate'), repos='https://cran.rstudio.com')
#library(devtools)
# 
# #start http-server (npm install http-server -g) on a suitable port
# #glmr_install     = clusterCall(c1, function(){install.packages('glmtools', repos=c('http://owi.usgs.gov/R', 'http://cran.rstudio.com'))})
# glmr_install     = clusterCall(c1, function(){install_url(paste0('http://', local_url,'/GLMr_3.1.10.tar.gz'))})
# glmtools_install = clusterCall(c1, function(){install_url(paste0('http://', local_url,'/glmtools_0.13.0.tar.gz'))})
# lakeattr_install = clusterCall(c1, function(){install_url(paste0('http://', local_url,'/lakeattributes_0.8.6.tar.gz'))})
# mdalakes_install = clusterCall(c1, function(){install_url(paste0('http://', local_url,'/mda.lakes_4.1.0.tar.gz'))})


#library(Rmpi)

#args = commandArgs(trailingOnly=TRUE)
mpirank = is.numeric(Sys.getenv('SLURM_PROCID', 'NA')) #mpi.comm.rank(0)
mpisize = is.numeric(Sys.getenv('SLURM_STEP_NUM_TASKS', 'NA')) #mpi.comm.size(0)
cat('MPIRANK:', mpirank, '\n')
cat('MPISIZE:', mpisize, '\n')

if(is.na(mpirank)|| is.na(mpisize)){
	stop('trouble finding MPIRANK or MPISIZE')
}


# if(mpi.comm.rank(0) != 0){
# 	cat("Launching proletariat on comm rank ", mpi.comm.rank(0), '\n')
# 	slaveLoop(makeMPImaster())
# 	q()
# }else{
# 	cat("Launching bourgeoisie on comm rank ", mpi.comm.rank(0), '\n')
# 	makeMPIcluster()
# }

# c1 = getMPIcluster()

library(lakeattributes)
library(mda.lakes)
library(dplyr)
library(glmtools)
source(system.file('demo/common_running_functions.R', package='mda.lakes'))

Sys.setenv(TZ='GMT')

# clusterEvalQ(c1, Sys.setenv(TZ='GMT'))


future_hab_wtr = function(site_id, modern_era=1979:2012, future_era, driver_function=get_driver_path, secchi_function=function(site_id){}, nml_args=list()){
	
	library(lakeattributes)
	library(mda.lakes)
	library(dplyr)
	library(glmtools)
  library(lubridate)
	
	fastdir = tempdir()
	#for use on WiWSC Condor pool
	if(file.exists('/mnt/ramdisk')){
		fastdir = '/mnt/ramdisk'
	}
	#for use on YETI
	if(Sys.getenv('RAM_SCRATCH', unset = '') != ''){
		fastdir = Sys.getenv('RAM_SCRATCH', unset = '')
	}
	
	
	tryCatch({
		
		
		run_dir = file.path(fastdir, paste0(site_id, '_', sample.int(1e9, size=1)))
		cat('START:', format(Sys.time(), '%m-%d %H:%M:%S'), Sys.info()[['nodename']], site_id, '\n')
		dir.create(run_dir)
		
		#rename for dplyr
		nhd_id = site_id
		
		#prep observations for calibration data
		data(wtemp)
		obs = filter(wtemp, site_id == nhd_id) %>%
		  transmute(DateTime=date, Depth=depth, temp=wtemp) %>%
		  filter(year(DateTime) %in% modern_era)
		
		have_cal = nrow(obs) > 0
		
		if(have_cal){
		  #having a weird issue with resample_to_field, make unique
		  obs = obs[!duplicated(obs[,1:2]), ]
		  
		  write.table(obs, file.path(run_dir, 'obs.tsv'), sep='\t', row.names=FALSE)
		}
		
		#get driver data
		driver_path = driver_function(site_id)
		driver_path = gsub('\\\\', '/', driver_path)
		
		
		#kds = get_kd_best(site_id, years=years, datasource = datasource)
		
		kd_avg = secchi_function(site_id) #secchi_conv/mean(kds$secchi_avg, na.rm=TRUE)
		
		#run with different driver and ice sources
		
		prep_run_glm_kd(site_id=site_id, 
										path=run_dir, 
										years=modern_era,
										kd=kd_avg, 
										nml_args=c(list(
											dt=3600, subdaily=FALSE, nsave=24, 
											timezone=-6,
											csv_point_nlevs=0, 
											snow_albedo_factor=1.1, 
											meteo_fl=driver_path, 
											cd=getCD(site_id, method='Hondzo')), 
											nml_args))
		
		
		##parse the habitat and WTR info. next run will clobber output.nc
		wtr_all = get_temp(file.path(run_dir, 'output.nc'), reference='surface')
		## drop the first n burn-in years
		#years = as.POSIXlt(wtr$DateTime)$year + 1900
		#to_keep = !(years <= min(years) + nburn - 1)
		#wtr_all = wtr[to_keep, ]
		
		core_metrics = necsc_thermal_metrics_core(run_dir, site_id)
		
		hansen_habitat = hansen_habitat_calc(run_dir, site_id)
		
		#notaro_metrics = summarize_notaro(paste0(run_dir, '/output.nc'))
		
		nml = read_nml(file.path(run_dir, "glm2.nml"))
		
		## if we have cal, use it
		if(have_cal){
		  cal_data = resample_to_field(file.path(run_dir, 'output.nc'), file.path(run_dir,'obs.tsv'))
		  cal_data$site_id = site_id
		  cat('Calibration data calculated\n')
		}else{
		  cal_data = data.frame() #just use empy data frame if no cal data
		  cat('No Cal, calibration skipped\n')
		}
		
		unlink(run_dir, recursive=TRUE)
		
		#notaro_metrics$site_id = site_id
		
		all_data = list(wtr=wtr_all, core_metrics=core_metrics, 
		                hansen_habitat=hansen_habitat, 
		                site_id=site_id, 
		                #notaro_metrics=notaro_metrics, 
		                nml=nml, 
		                cal_data=cal_data)
		
		cat('END:', format(Sys.time(), '%m-%d %H:%M:%S'), Sys.info()[['nodename']], site_id, '\n')
		
		return(all_data)
		
	}, error=function(e){
		unlink(run_dir, recursive=TRUE);
	  cat('FAIL:', format(Sys.time(), '%m-%d %H:%M:%S'), Sys.info()[['nodename']], site_id, '\n')
		return(list(error=e, site_id))
	})
}


driver_fun = function(site_id){
	nldas = read.csv(get_driver_path(site_id, driver_name = 'NLDAS'), header=TRUE)
	drivers = driver_nldas_wind_debias(nldas)
	drivers = driver_add_burnin_years(drivers, nyears=2)
	drivers = driver_add_rain(drivers, month=7:9, rain_add=0.5) ##keep the lakes topped off
	driver_save(drivers)
}

getnext = function(fname){
	i=0
	barefname = fname
	while(file.exists(fname)){
		i=i+1
		fname = paste0(barefname, '.', i)
	}
	return(fname)
}

wrapup_output = function(out, run_name, years){
	out_dir = file.path('.', run_name)
	
	run_exists = file.exists(out_dir)
	
	if(!run_exists) {dir.create(out_dir, recursive=TRUE)}
	
	good_data = out[!unlist(lapply(out, function(x){'error' %in% names(x) || is.null(x)}))]
	bad_data  = out[unlist(lapply(out, function(x){'error' %in% names(x) || is.null(x)}))]
	
	sprintf('%i lakes ran\n', length(good_data))
	dframes = lapply(good_data, function(x){tmp = x[[1]]; tmp$site_id=x[['site_id']]; return(tmp)})
	#drop the burn-in years
	dframes = lapply(dframes, function(df){subset(df, DateTime > as.POSIXct('1979-01-01'))})
	
	hansen_habitat = do.call(rbind, lapply(good_data, function(x){x[['hansen_habitat']]}))
	hansen_habitat = subset(hansen_habitat, year %in% years)
	
	core_metrics = do.call(rbind, lapply(good_data, function(x){x[['core_metrics']]}))
	core_metrics = subset(core_metrics, year %in% years)
	
	cal_data = do.call(rbind, lapply(good_data, function(x){x[['cal_data']]}))
	
	model_config = lapply(good_data, function(x){x$nml})
	
	write.table(hansen_habitat, file.path(out_dir, 'best_hansen_hab.tsv'), sep='\t', row.names=FALSE, append=run_exists, col.names=!run_exists)
	write.table(core_metrics, file.path(out_dir, 'best_core_metrics.tsv'), sep='\t', row.names=FALSE, append=run_exists, col.names=!run_exists)
	write.table(cal_data, file.path(out_dir, 'best_cal_data.tsv'), sep='\t', row.names=FALSE, append=run_exists, col.names=!run_exists)
	
	save('dframes', file = getnext(file.path(out_dir, 'best_all_wtr.Rdata')))
	save('bad_data', file = getnext(file.path(out_dir, 'bad_data.Rdata')))
	save('model_config', file=getnext(file.path(out_dir, 'model_config.Rdata')))
	
	rm(out, good_data, dframes)
	gc()
}


################################################################################
## Lets run NLDAS  1979:2015
################################################################################
driver_fun = function(site_id){
  nldas = read.csv(get_driver_path(site_id, driver_name = 'NLDAS'), header=TRUE)
  drivers = driver_nldas_wind_debias(nldas)
  drivers = driver_add_burnin_years(drivers, nyears=2)
  drivers = driver_add_rain(drivers, month=7:9, rain_add=0.5) ##keep the lakes topped off
  driver_save(drivers)
}

config = read.table('config', header=TRUE, as.is=TRUE)

driver_name = config$drivername

driver_url = config$driverurl

to_run = as.character(unique(zmax$site_id))
to_run = split(to_run, cut(seq_along(to_run), mpisize, labels = FALSE))[[mpirank+1]]

#clusterExport(c1, 'driver_fun')
#clusterExport(c1, 'secchi_standard')
#clusterExport(c1, 'driver_name')
#clusterExport(c1, 'driver_url')
#clusterCall(c1, function(){library(mda.lakes);set_driver_url(driver_url)})
set_driver_url(driver_url)

run_name = paste0(mpirank)

##1980-1999
runsplits = split(1:length(to_run), floor(1:length(to_run)/1e3))
yeargroups = list(1979:2015)

for(ygroup in yeargroups){
	for(rsplit in runsplits){
		start = Sys.time()
		out = lapply(to_run[rsplit], future_hab_wtr, 
												 modern_era=ygroup, 
												 secchi_function=secchi_standard,
												 driver_function=function(site_id){driver_fun(site_id)})
		
		wrapup_output(out, run_name, years=ygroup)
		
		print(difftime(Sys.time(), start, units='hours'))
		cat('on to the next\n')
		cat(rsplit, '\n')
		cat(ygroup, '\n')
	}
}



