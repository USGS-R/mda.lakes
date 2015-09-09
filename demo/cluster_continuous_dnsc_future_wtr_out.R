## Lets cluserify things

library(parallel)

#lets try 100 to start
c1 = makePSOCKcluster(paste0('licon', 1:50), manual=TRUE, port=4042)


clusterCall(c1, function(){install.packages('devtools', repos='http://cran.rstudio.com')})
clusterCall(c1, function(){install.packages('rLakeAnalyzer', repos='http://cran.rstudio.com')})
clusterCall(c1, function(){install.packages('dplyr', repos='http://cran.rstudio.com')})
clusterCall(c1, function(){install.packages('lubridate', repos='http://cran.rstudio.com')})


clusterCall(c1, function(){library(devtools)})

glmr_install     = clusterCall(c1, function(){install_github('lawinslow/GLMr')})
glmtools_install = clusterCall(c1, function(){install_github('lawinslow/glmtools')})
lakeattr_install = clusterCall(c1, function(){install_github('lawinslow/lakeattributes')})
mdalakes_install = clusterCall(c1, function(){install_github('lawinslow/mda.lakes')})

library(lakeattributes)
library(mda.lakes)
library(dplyr)
library(glmtools)
library(reshape2)

Sys.setenv(TZ='GMT')

#duplicate two years of initial data
get_driver_with_burnin = function(fname, driver_name, nyears=2){
	drivers = read.csv(get_driver_path(fname, driver_name), header=TRUE)
	drivers$time = as.POSIXct(drivers$time)
	
	for(i in 1:nyears){
		to_dup = drivers[1:365, ]
		to_dup$time = to_dup$time - as.difftime(365, units='days')  #drop 365 days from the date/time col
		drivers = rbind(to_dup, drivers)
	}
	
	new_fpath = tempfile(fileext='.csv')
	write.table(drivers, new_fpath, quote=FALSE, row.names=FALSE, col.names=TRUE, sep=',')
	return(new_fpath)
}


lakes = read.table(system.file('supporting_files/managed_lake_info.txt', package = 'mda.lakes'), 
                   sep='\t', quote="\"", header=TRUE, as.is=TRUE, colClasses=c(WBIC='character'))

to_run = paste0('WBIC_', lakes$WBIC)


future_wtr_out = function(site_id){
  nburn = 2
  modern_era = 1977:1999 #with 2 burn-in years
  future_era = 2040:2069 #will drop first two years
  
  library(lakeattributes)
  library(mda.lakes)
  library(dplyr)
  library(glmtools)
  library(stringr)
  
  tryCatch({
    fastdir = tempdir()
    if(file.exists('/mnt/ramdisk')){
      fastdir = '/mnt/ramdisk'
    }
    
    run_dir = file.path(fastdir, site_id)
    dir.create(run_dir)
    cat(run_dir, '\n')
    
    secchi = get_kd_best(site_id, years=modern_era)
    
    bare_wbic = substr(site_id, 6, nchar(site_id))
    
    driver_path = get_driver_with_burnin(paste0(site_id, '.csv'), 'ECHAM5', nyears=nburn)
    driver_path = gsub('\\\\', '/', driver_path)
    
    #run with different driver and ice sources
    
    prep_run_glm_kd(bare_wbic, kd=1.7/secchi$secchi_avg, path=run_dir, years=modern_era,
                            nml_args=list(
                              dt=3600, subdaily=FALSE, nsave=24, 
                              timezone=-6,
                              csv_point_nlevs=0, 
                              meteo_fl=driver_path, 
                              snow_albedo_factor=1.1))
    
    
    ##parse the habitat and WTR info. next run will clobber output.nc
    wtr = get_temp(file.path(run_dir, 'output.nc'), reference='surface')
    ## drop the first n burn-in years
    years = as.POSIXlt(wtr$DateTime)$year + 1900
    to_keep = !(years <= min(years) + nburn - 1)
    wtr_all = wtr[to_keep, ]
    
    secchi = get_kd_best(site_id, years=future_era)
    
    prep_run_glm_kd(bare_wbic, kd=1.7/secchi$secchi_avg, path=run_dir, years=future_era,
                            nml_args=list(
                              dt=3600, subdaily=FALSE, nsave=24, 
                              timezone=-6,
                              csv_point_nlevs=0, 
                              meteo_fl=driver_path,
                              snow_albedo_factor=1.1))
    
    wtr = get_temp(file.path(run_dir, 'output.nc'), reference='surface', z_out = get.offsets(wtr))
    ## drop the first n burn-in years
    years = as.POSIXlt(wtr$DateTime)$year + 1900
    to_keep = !(years <= min(years) + nburn - 1)
    wtr = wtr[to_keep, ]
    
    wtr_all = rbind(wtr_all, wtr)
    
    
    unlink(run_dir, recursive=TRUE)
    
    all_data = list(wtr_all, site_id)

    return(all.data)
    
  }, error=function(e){unlink(run_dir, recursive=TRUE);e})
}


out = clusterApplyLB(c1, to_run, future_wtr_out)

dframes = out[unlist(lapply(out, inherits, what='data.frame'))]

save('dframes', file = '~/FUTURE_ECHAM5.Rdata')

out_path = '~/FUTURE_ECHAM5'
dir.create(out_path)
  
for(i in 1:length(dframes)){
  
  cat(i, '\n')  
  d = dframes[[i]]
  site_id = d$site_id[1]
  d$site_id = NULL
  tmp = gzfile(paste0(out_path, '/', site_id, '.tsv.gz'))
  write.table(d, tmp , sep='\t', row.names=FALSE, quote=FALSE)
}




