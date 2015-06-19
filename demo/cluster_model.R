## Lets cluserify things

library(parallel)

#lets try 30 to start
c1 = makePSOCKcluster(paste0('licon', 1:100), manual=TRUE, port=4043)


clusterCall(c1, function(){install.packages('devtools', repos='http://cran.rstudio.com')})
clusterCall(c1, function(){install.packages('rLakeAnalyzer', repos='http://cran.rstudio.com')})
clusterCall(c1, function(){install.packages('dplyr', repos='http://cran.rstudio.com')})


clusterCall(c1, function(){library(devtools)})

glmr_install     = clusterCall(c1, function(){install_github('lawinslow/GLMr')})
glmtools_install = clusterCall(c1, function(){install_github('lawinslow/glmtools')})
lakeattr_install = clusterCall(c1, function(){install_github('lawinslow/lakeattributes')})
mdalakes_install = clusterCall(c1, function(){install_github('lawinslow/mda.lakes')})

library(lakeattributes)
library(mda.lakes)
library(dplyr)
library(glmtools)

obs = read.table(system.file('supporting_files/wtemp.obs.tsv', package = 'mda.lakes'), 
                 sep='\t', header=TRUE, as.is=TRUE, , colClasses=c(WBIC='character'))
obs$site_id = paste0('WBIC_', obs$WBIC)

secchi_obs = filter(secchi, source=='in-situ')

#currently using in-situ secchi data, switch to all?
#best_clarity = intersect(secchi_obs$site_id, turbidity$site_id)
to_run = intersect(obs$site_id, secchi_obs$site_id)


run_cal = function(site_id){

  
  library(lakeattributes)
  library(mda.lakes)
  library(dplyr)
  library(glmtools)
  
  fastdir = tempdir()
  if(file.exists('/mnt/ramdisk')){
    fastdir = '/mnt/ramdisk'
  }
  
  secchi_conv = 1.7
  tryCatch({
    
    get_driver_path(paste0(site_id, '.csv'))
    
    run_dir = file.path(fastdir, site_id)
    dir.create(run_dir)
    
    bare_wbic = substr(site_id, 6, nchar(site_id))
    
    obs = read.table(system.file('supporting_files/wtemp.obs.tsv', package = 'mda.lakes'), 
                     sep='\t', header=TRUE, as.is=TRUE, , colClasses=c(WBIC='character'))
    obs$site_id = paste0('WBIC_', obs$WBIC)
    
    lake_obs = obs[obs$WBIC == bare_wbic, c('DATETIME', 'DEPTH', 'WTEMP')]
    write.table(lake_obs, file.path(run_dir, 'obs.tsv'), sep='\t', row.names=FALSE)
    
    kds = get_kd_year(site_id, src='in-situ')
    kds = group_by(kds, year) %>% summarise(secchi_m = mean(secchi_m)) %>%
      filter(year >= 1979, year <= 2012)
    
    kd_avg = secchi_conv/mean(get_kd_year(site_id, src='in-situ')$secchi_m, na.rm=TRUE)
    
    prep_run_chained_glm_kd(site_id=bare_wbic, 
                            path=run_dir, 
                            years=1979:2012, 
                            kd=kd_avg, 
                            nml_args=list(
                              dt=3600, subdaily=FALSE, nsave=24, 
                              timezone=-6,
                              csv_point_nlevs=0))
    
    sims = Sys.glob(file.path(run_dir, 'output*.nc'))
    #browser()
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
  
  }, error=function(e){e})
}


#out = clusterApplyLB(c1, to_run[1:100], run_cal)


################################################################################
# Run comparison between turbidity and secchi calculated clarity
################################################################################
obs = read.table(system.file('supporting_files/wtemp.obs.tsv', package = 'mda.lakes'), 
                 sep='\t', header=TRUE, as.is=TRUE, , colClasses=c(WBIC='character'))
obs$site_id = paste0('WBIC_', obs$WBIC)

secchi_obs = filter(secchi, source=='in-situ')

turb_obs = intersect(obs$site_id, turbidity$site_id)
to_run = intersect(secchi_obs$site_id, turb_obs)

run_cal_turb = function(site_id){

  library(lakeattributes)
  library(mda.lakes)
  library(dplyr)
  library(glmtools)
  
  turb_avg    = get_turbidity_avg(site_id)
  year_secchi = get_kd_year(site_id, src = 'in-situ') %>%
    group_by(year) %>% summarise(secchi_avg=mean(secchi_m)) %>% 
    filter(year <= 2012 & year >= 1979)
  
  
  kd = 0.29 + 0.09 * turb_avg$turbidity_avg * year_secchi$secchi_avg
  kd_1p7 = 1.7/year_secchi$secchi_avg

  
  fastdir = tempdir()
  if(file.exists('/mnt/ramdisk')){
    fastdir = '/mnt/ramdisk'
  }
  
  secchi_conv = 1.7
  
  tryCatch({
    get_driver_path(paste0(site_id, '.csv'))
    
    
    run_dir = file.path(fastdir, site_id)
    dir.create(run_dir)
    
    bare_wbic = substr(site_id, 6, nchar(site_id))
    
    obs = read.table(system.file('supporting_files/wtemp.obs.tsv', package = 'mda.lakes'), 
                     sep='\t', header=TRUE, as.is=TRUE, , colClasses=c(WBIC='character'))
    obs$site_id = paste0('WBIC_', obs$WBIC)
    
    lake_obs = obs[obs$WBIC == bare_wbic, c('DATETIME', 'DEPTH', 'WTEMP')]
    write.table(lake_obs, file.path(run_dir, 'obs.tsv'), sep='\t', row.names=FALSE)
    
    
    prep_run_chained_glm_kd(site_id=bare_wbic, 
                            path=run_dir, 
                            years=year_secchi$year, 
                            kd=kd_1p7, 
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
  
  }, error=function(e){e})
}

out_turb = clusterApplyLB(c1, to_run, run_cal_turb)

out_1p7  = clusterApplyLB(c1, to_run, run_cal)

res_turb = data.frame()
res_1p7  = data.frame()


res_turb = do.call('rbind', out_turb[unlist(lapply(out_turb, inherits, what='data.frame'))])
res_1p7  = do.call('rbind', out_1p7[unlist(lapply(out_1p7, inherits, what='data.frame'))])

sqrt(mean((res_turb$Observed_wTemp - res_turb$Modeled_wTemp)^2, na.rm=TRUE))
sqrt(mean((res_1p7$Observed_wTemp - res_1p7$Modeled_wTemp)^2, na.rm=TRUE))

