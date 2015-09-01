## Lets cluserify things

library(parallel)

#lets try 100 to start
c1 = makePSOCKcluster(paste0('licon', 1:50), manual=TRUE, port=4044)


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
library(lubridate)
library(mda.lakes)
library(dplyr)
library(glmtools)


nldas_wind_debias = function(nldas_path, dbiased_path){
  
  
  nldas = read.csv(nldas_path, header=TRUE)
  nldas$time = as.POSIXct(nldas$time)
  
  after_2001 = nldas$time > as.POSIXct('2001-12-31')
  
  nldas$WindSpeed[after_2001] = nldas$WindSpeed[after_2001] * 0.921
  
  if(missing(dbiased_path)){
    return(nldas)
  }else{
    write.csv(nldas, dbiased_path, row.names=FALSE, quote=FALSE)
  }
}
clusterExport(c1, varlist = 'nldas_wind_debias')

obs = read.table(system.file('supporting_files/wtemp.obs.tsv', package = 'mda.lakes'), 
                 sep='\t', header=TRUE, as.is=TRUE, , colClasses=c(WBIC='character'))
to_run = unique(paste0('WBIC_', obs$WBIC))


downscale_cal_out = function(site_id){
  
  modern_era = 1979:1999
  future_era = 2040:2069
  
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
    
    bare_wbic = substr(site_id, 6, nchar(site_id))
    
    
    obs = read.table(system.file('supporting_files/wtemp.obs.tsv', package = 'mda.lakes'), 
                     sep='\t', header=TRUE, as.is=TRUE, , colClasses=c(WBIC='character'))
    obs$site_id = paste0('WBIC_', obs$WBIC)
    
    
    lake_obs = obs[obs$WBIC == bare_wbic, c('DATETIME', 'DEPTH', 'WTEMP')]
    lake_obs$year = as.POSIXlt(lake_obs$DATETIME)$year+1900
    
    write.table(lake_obs, file.path(run_dir, 'obs.tsv'), sep='\t', row.names=FALSE)
    years = unique(lake_obs$year)
    years = years[years <= 2012 & years >=1979]
    
    cat(years)
    
    secchi = get_kd_best(site_id, years=years)
    
    #driver_path = get_driver_path(paste0(site_id, '.csv'), 'NLDAS', loc_cache=FALSE)
    driver_path = tempfile(fileext='.csv')
    nldas_wind_debias(get_driver_path(paste0(site_id, '.csv'), 'NLDAS'),
                dbiased_path=driver_path)
    
    driver_path = gsub('\\\\', '/', driver_path)
    
    #run with different driver and ice sources
    
    res = prep_run_chained_glm_kd(bare_wbic, kd=1.7/secchi$secchi_avg, path=run_dir, years=years,
                            verbose=TRUE,
                            nml_args=list(
                              dt=3600, subdaily=FALSE, nsave=24, 
                              timezone=-6,
                              csv_point_nlevs=0, 
                              meteo_fl=driver_path))
    
    
    sims = Sys.glob(file.path(run_dir, 'output*.nc'))
    if(length(sims) < 1){
      stop('No successful sims')
    }
    #loop over years
    cal.data = data.frame()
    for(i in 1:length(sims)){
      tryCatch({
        tmp = NULL
        tmp = resample_to_field(sims[i], file.path(run_dir,'obs.tsv'))
      }, error=function(e){e})
      if(!is.null(tmp)){
        cal.data = rbind(cal.data, tmp)
      }
    }
    unlink(run_dir, recursive=TRUE)
    
    cal.data
    
  }, error=function(e){unlink(run_dir, recursive=TRUE);e})
}

groups = split(to_run, ceiling(seq_along(to_run)/100))
out = list()
for(grp in groups){
  tmp = clusterApplyLB(c1,grp, downscale_cal_out)
  out = c(out, tmp)
  cat('iteration\n')
}
#out = clusterApplyLB(c1, 'WBIC_394400', downscale_cal_out)


all_cal = do.call('rbind', out[unlist(lapply(out, inherits, what='data.frame'))])

mean(all_cal$Observed_wTemp - all_cal$Modeled_wTemp, na.rm=TRUE)

sqrt(mean((all_cal$Observed_wTemp - all_cal$Modeled_wTemp)^2, na.rm=TRUE))
