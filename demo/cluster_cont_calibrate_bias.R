## Lets cluserify things
#'checklist
#'1. Start local web server to serve large packages
#'2. Set local variable "local_url" to web server address:port
#'   
#local_url = '192.0.2.1:4040'

local_url = paste0((Sys.info()["nodename"]),':4040')
library(parallel)

#lets try 50 to start
c1 = makePSOCKcluster(paste0('licon', 1:50), manual=TRUE, port=4042)

clusterExport(c1, varlist = 'local_url')

#clusterCall(c1, function(){install.packages('devtools', repos='http://cran.rstudio.com')})
clusterCall(c1, function(){install.packages('rLakeAnalyzer', repos='http://cran.rstudio.com')})
clusterCall(c1, function(){install.packages('dplyr', repos='http://cran.rstudio.com')})
clusterCall(c1, function(){install.packages('lubridate', repos='http://cran.rstudio.com')})

clusterCall(c1, function(){library(devtools)})

#start http-server (npm install http-server -g) on a suitable port

glmr_install     = clusterCall(c1, function(){install_url(paste0('http://', local_url,'/GLMr_3.1.4.tar.gz'))})
glmtools_install = clusterCall(c1, function(){install_github('lawinslow/glmtools')})
lakeattr_install = clusterCall(c1, function(){install_url(paste0('http://', local_url,'/lakeattributes_0.3.0.tar.gz'))})
mdalakes_install = clusterCall(c1, function(){install_url(paste0('http://', local_url,'/mda.lakes_2.10.4.tar.gz'))})

library(lakeattributes)
library(mda.lakes)
library(dplyr)
library(glmtools)

obs = read.table(system.file('supporting_files/wtemp.obs.tsv', package = 'mda.lakes'), 
                 sep='\t', header=TRUE, as.is=TRUE, , colClasses=c(WBIC='character'))
obs$site_id = paste0('WBIC_', obs$WBIC)

#secchi_obs = filter(secchi, source=='in-situ')

#currently using in-situ secchi data, switch to all?
#best_clarity = intersect(secchi_obs$site_id, turbidity$site_id)
#to_run = intersect(obs$site_id, secchi_obs$site_id)
to_run = unique(obs$site_id)


run_cal = function(site_id, years=1979:2012, driver_function=get_driver_path, nml_args=list()){

  
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
    
    #get_driver_path(paste0(site_id, '.csv'))
    
    run_dir = file.path(fastdir, site_id)
    dir.create(run_dir)
    
    bare_wbic = substr(site_id, 6, nchar(site_id))
    
    obs = read.table(system.file('supporting_files/wtemp.obs.tsv', package = 'mda.lakes'), 
                     sep='\t', header=TRUE, as.is=TRUE, , colClasses=c(WBIC='character'))
    obs$site_id = paste0('WBIC_', obs$WBIC)
    
    lake_obs = obs[obs$WBIC == bare_wbic, c('DATETIME', 'DEPTH', 'WTEMP')]
    #having a weird issue with resample_to_field, make unique
    lake_obs = lake_obs[!duplicated(lake_obs[,1:2]), ]
    
    
    write.table(lake_obs, file.path(run_dir, 'obs.tsv'), sep='\t', row.names=FALSE)
    
    driver_path = driver_function(site_id)
    driver_path = gsub('\\\\', '/', driver_path)
    
    
    kds = get_kd_best(site_id, years=years)
    
    kd_avg = secchi_conv/mean(kds$secchi_avg, na.rm=TRUE)
    
    prep_run_glm_kd(site_id=bare_wbic, 
                            path=run_dir, 
                            years=years,
                            kd=kd_avg, 
                            nml_args=c(list(
                              dt=3600, subdaily=FALSE, nsave=24, 
                              timezone=-6,
                              csv_point_nlevs=0, 
                              snow_albedo_factor=1.1, 
                              meteo_fl=driver_path), 
                              nml_args))
    
    cal.data = resample_to_field(file.path(run_dir, 'output.nc'), file.path(run_dir,'obs.tsv'))
    
    unlink(run_dir, recursive=TRUE)
    
    return(cal.data)
  
  }, error=function(e){unlink(run_dir, recursive=TRUE);e})
}


driver_fun = function(site_id){
  nldas = read.csv(get_driver_path(paste0(site_id, '.csv'), driver_name = 'NLDAS'), header=TRUE)
  drivers = driver_nldas_wind_debias(nldas)
  drivers = driver_add_burnin_years(drivers, nyears=2)
  drivers = driver_add_rain(drivers, month=7:9, rain_add=0.5) ##keep the lakes topped off
  driver_save(drivers)
}

#we want only ramdisk enabled nodes
ramdisk = clusterCall(c1, function(){file.exists('/mnt/ramdisk')})
c1 = c1[unlist(ramdisk)]

out = clusterApplyLB(c1, to_run, run_cal, driver_function = driver_fun, nml_args=list())
#
# groups = split(to_run, ceiling(seq_along(to_run)/100))
# out = list()
# for(grp in groups){
#   tmp = clusterApplyLB(c1,grp, run_cal, driver_fun)
#   out = c(out, tmp)
#   cat('iteration\n')
# }

#out = clusterApplyLB(c1, to_run[1:50], run_cal)

sprintf('%i lakes ran\n', sum(unlist(lapply(out, inherits, what='data.frame'))))

##results
out_df = do.call('rbind', out[unlist(lapply(out, inherits, what='data.frame'))])

sqrt(mean((out_df$Observed_wTemp - out_df$Modeled_wTemp)^2, na.rm=TRUE))
mean(out_df$Observed_wTemp - out_df$Modeled_wTemp, na.rm=TRUE)

################################################################################
## GENMOM
################################################################################

groups = split(to_run, ceiling(seq_along(to_run)/100))
out = list()
for(grp in groups){
  tmp = clusterApplyLB(c1, grp, run_cal, years=1980:1999, driver_name='GENMOM')
  out = c(out, tmp)
  cat('iteration\n')
}

#out = clusterApplyLB(c1, to_run[1:50], run_cal)

##results
out_df = do.call('rbind', out[unlist(lapply(out, inherits, what='data.frame'))])
sqrt(mean((out_df$Observed_wTemp - out_df$Modeled_wTemp)^2, na.rm=TRUE))
mean(out_df$Observed_wTemp - out_df$Modeled_wTemp, na.rm=TRUE)


