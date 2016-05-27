## Lets cluserify things
#'checklist
#'1. Start local web server to serve large packages
#'2. Set local variable "local_url" to web server address:port
#'   
#local_url = '192.0.2.1:4040'

local_url = paste0((Sys.info()["nodename"]),':4040')
driver_url = paste0('http://', (Sys.info()["nodename"]),':4041/')

library(parallel)

#lets try 50 to start
c1 = makePSOCKcluster(paste0('licon', 1:60), manual=TRUE, port=4046)

clusterExport(c1, varlist = 'local_url')
clusterExport(c1, varlist = 'driver_url')

#clusterCall(c1, function(){install.packages('devtools', repos='http://cran.rstudio.com')})
clusterCall(c1, function(){install.packages('rLakeAnalyzer', repos='http://cran.rstudio.com')})
clusterCall(c1, function(){install.packages('dplyr', repos='http://cran.rstudio.com')})
clusterCall(c1, function(){install.packages('lubridate', repos='http://cran.rstudio.com')})

clusterCall(c1, function(){library(devtools)})

#start http-server (npm install http-server -g) on a suitable port
#glmr_install     = clusterCall(c1, function(){install.packages('glmtools', repos=c('http://owi.usgs.gov/R', 'http://cran.rstudio.com'))})
glmr_install     = clusterCall(c1, function(){install_url(paste0('http://', local_url,'/GLMr_3.1.10.tar.gz'))})
glmtools_install = clusterCall(c1, function(){install_url(paste0('http://', local_url,'/glmtools_0.13.0.tar.gz'))})
lakeattr_install = clusterCall(c1, function(){install_url(paste0('http://', local_url,'/lakeattributes_0.8.6.tar.gz'))})
mdalakes_install = clusterCall(c1, function(){install_url(paste0('http://', local_url,'/mda.lakes_3.0.4.tar.gz'))})

library(lakeattributes)
library(mda.lakes)
library(dplyr)
library(glmtools)
library(plyr)
## setup run function

run_cal = function(site_id, years=1979:2012, driver_function=get_driver_path, nml_args=list(), secchi_multi=1, cd_multi=1){
  
  
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
    
    
    run_dir = file.path(fastdir, paste0(site_id, '_', sample.int(1e9, size=1)))
    cat(run_dir, '\n')
    dir.create(run_dir)
    
    #rename for dplyr
    nhd_id = site_id
    
    data(wtemp)
    obs = filter(wtemp, site_id == nhd_id) %>%
      transmute(DateTime=date, Depth=depth, temp=wtemp)
    
    #having a weird issue with resample_to_field, make unique
    obs = obs[!duplicated(obs[,1:2]), ]
    
    write.table(obs, file.path(run_dir, 'obs.tsv'), sep='\t', row.names=FALSE)
    
    #get driver data
    driver_path = driver_function(site_id)
    driver_path = gsub('\\\\', '/', driver_path)
    
    
    kds = get_kd_best(site_id, years=years)
    # kds$month = as.POSIXlt(kds$date)$mon + 1
    # kds = subset(kds, month %in% c(7,8,9))
    # if(nrow(kds)){
    #   stop('no summertime kds')
    # }
    
    kd_avg = secchi_conv/(median(kds$secchi_avg, na.rm=TRUE)*secchi_multi)
    
    prep_run_glm_kd(site_id=site_id, 
                    path=run_dir, 
                    years=years,
                    kd=kd_avg, 
                    nml_args=c(list(
                      dt=3600, subdaily=FALSE, nsave=24, 
                      timezone=-6,
                      csv_point_nlevs=0, 
                      snow_albedo_factor=1.1, 
                      meteo_fl=driver_path, 
                      cd=cd_multi*getCD(site_id, method='Hondzo')), 
                      nml_args))
    
    cal.data = resample_to_field(file.path(run_dir, 'output.nc'), file.path(run_dir,'obs.tsv'))
    cal.data$site_id = site_id
    
    nml = read_nml(file.path(run_dir, "glm2.nml"))
    
    unlink(run_dir, recursive=TRUE)
    
    return(list(cal.data=cal.data, nml=nml))
    
  }, error=function(e){
    unlink(run_dir, recursive=TRUE)
    return(paste(e$call, e$message, Sys.info()["nodename"]))
  })
}


driver_fun = function(site_id){
  nldas = read.csv(get_driver_path(site_id, driver_name = 'NLDAS', timestep = 'daily'), header=TRUE)
  drivers = driver_nldas_wind_debias(nldas)
  drivers = driver_add_burnin_years(drivers, nyears=2)
  drivers = driver_add_rain(drivers, month=7:9, rain_add=0.5) ##keep the lakes topped off
  #drivers$time = drivers$time - as.difftime(2, units='days')
  driver_save(drivers)
}





#multipliers = data.frame(secchi_multi=c(1, 1, 1, 1, 1, 1, 1.1, 1.2, 1.3, 1.4, 1.5), cd_multi=c(1.0, 1.02, 1.04, 1.06, 1.08, 1.1, 1, 1, 1, 1, 1))
multipliers = data.frame(secchi_multi=c(1), cd_multi=c(1))

for(i in 1:nrow(multipliers)){
  
  secchi_multi = multipliers$secchi_multi[i]
  cd_multi     = multipliers$cd_multi[i]
    
  data(wtemp)
  enough_wtemp = ddply(wtemp, 'site_id', function(df){length(unique(df$date))})
  enough_wtemp = subset(enough_wtemp, V1 >=10)
  
  to_run = unique(intersect(intersect(enough_wtemp$site_id, zmax$site_id), secchi$site_id))
  
  
  run_name = paste0('2016-04-08_BATHYFIX_cal_NLDAS_secchix', secchi_multi, '_cdx', cd_multi)
  run_comment = paste0("Markfort sheltering, full cal, lakes > 10 dates obs, JAS secchi. 1980-2014. ", 'secchix:', secchi_multi, ' cdx:', cd_multi)
  
  clusterCall(c1, function(){library(mda.lakes);set_driver_url(driver_url)})
  
  out = clusterApplyLB(c1, to_run, run_cal, driver_function = driver_fun, nml_args=list(), years=1980:2014, secchi_multi=secchi_multi, cd_multi=cd_multi)
  
  sprintf('%i lakes ran\n', sum(unlist(lapply(out, inherits, what='list'))))
  
  nmls = lapply(out[sapply(out, inherits, what='list')], function(x){x$nml})
  out_err = out[!unlist(lapply(out, inherits, what='list'))]
  out = lapply(out[sapply(out, inherits, what='list')], function(x){x$cal.data})
  
  ##results
  out_df = do.call('rbind', out[unlist(lapply(out, inherits, what='data.frame'))])
  write.table(out_df, paste0('c:/WiLMA/results/', run_name, '.tsv'), sep='\t', row.names=FALSE)
  
  save(out_err, file = paste0('c:/WiLMA/results/', run_name, '.Rdata'))
  save(nmls, file=paste0('c:/WiLMA/results/', run_name, 'nml.Rdata'))
  
  rmarkdown::render('demo/document_calibration_overview_template.Rmd', output_file=paste0(run_name,'.html'),
                    output_dir='c:/WiLMA/results/', params=list(out_df=out_df, run_message=run_comment))
  
  
  sqrt(mean((out_df$Observed_temp - out_df$Modeled_temp)^2, na.rm=TRUE))
  mean(out_df$Observed_temp - out_df$Modeled_temp, na.rm=TRUE)
  
  parent = '5706a00ce4b032f77a8a477c'
  files = c(Sys.glob(paste0('c:/WiLMA/results/', run_name, '.*')), Sys.glob(paste0('c:/WiLMA/results/', run_name, 'nml.*')))
  
  library(sbtools)
  authenticate_sb('lwinslow@usgs.gov', pass)
  itm = item_create(parent, title=run_name)
  
  item_append_files(itm, files)

}

