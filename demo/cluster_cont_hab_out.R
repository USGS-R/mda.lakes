## Lets cluserify things

local_url = paste0((Sys.info()["nodename"]),':4040')
driver_url = paste0('http://', (Sys.info()["nodename"]),':4041/')

library(parallel)

#lets try 50 to start
c1 = makePSOCKcluster(paste0('licon', 1:60), manual=TRUE, port=4045)

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
mdalakes_install = clusterCall(c1, function(){install_url(paste0('http://', local_url,'/mda.lakes_4.1.0.tar.gz'))})

library(lakeattributes)
library(mda.lakes)
library(dplyr)
library(glmtools)
source('demo/common_running_functions.R')

Sys.setenv(TZ='GMT')


#lakes = read.table(system.file('supporting_files/managed_lake_info.txt', package = 'mda.lakes'), 
#                   sep='\t', quote="\"", header=TRUE, as.is=TRUE, colClasses=c(WBIC='character'))

#to_run = paste0('WBIC_', lakes$WBIC)


future_hab_wtr = function(site_id, modern_era=1979:2012, future_era, driver_function=get_driver_path, secchi_function=function(site_id){}, nml_args=list()){
  
  library(lakeattributes)
  library(mda.lakes)
  library(dplyr)
  library(glmtools)
  
  fastdir = tempdir()
  if(file.exists('/mnt/ramdisk')){
    fastdir = '/mnt/ramdisk'
  }
  
  
  tryCatch({
    
    
    run_dir = file.path(fastdir, paste0(site_id, '_', sample.int(1e9, size=1)))
    cat(run_dir, '\n')
    dir.create(run_dir)
    
    #rename for dplyr
    nhd_id = site_id
    
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
    
    #Run future era only if requested
    if(!missing(future_era)){
      kd_avg = secchi_function(site_id) #secchi_conv/mean(kds$secchi_avg, na.rm=TRUE)
      
      prep_run_glm_kd(site_id=site_id, 
                      path=run_dir, 
                      years=future_era,
                      kd=kd_avg, 
                      nml_args=c(list(
                        dt=3600, subdaily=FALSE, nsave=24, 
                        timezone=-6,
                        csv_point_nlevs=0, 
                        snow_albedo_factor=1.1, 
                        meteo_fl=driver_path, 
                        cd=getCD(site_id, method='Hondzo')), 
                        nml_args))
      
      wtr = get_temp(file.path(run_dir, 'output.nc'), reference='surface', z_out = get.offsets(wtr_all))
      ## drop the first n burn-in years
      #years = as.POSIXlt(wtr$DateTime)$year + 1900
      #to_keep = !(years <= min(years) + nburn - 1)
      #wtr = wtr[to_keep, ]
      
      wtr_all = rbind(wtr_all, wtr)
      
      core_metrics = rbind(core_metrics, necsc_thermal_metrics_core(run_dir, site_id))
      
      ##now hab
      hansen_habitat = rbind(hansen_habitat, hansen_habitat_calc(run_dir, lakeid=site_id))
    }
    
    unlink(run_dir, recursive=TRUE)
    
    all_data = list(wtr=wtr_all, core_metrics=core_metrics, hansen_habitat=hansen_habitat, site_id=site_id)

    return(all_data)
    
  }, error=function(e){
      unlink(run_dir, recursive=TRUE);
      return(list(error=e, site_id))
    })
}

################################################################################
## Lets run nldas
################################################################################
driver_fun = function(site_id){
  nldas = read.csv(get_driver_path(site_id, driver_name = 'NLDAS'), header=TRUE)
  drivers = driver_nldas_wind_debias(nldas)
  drivers = driver_add_burnin_years(drivers, nyears=2)
  drivers = driver_add_rain(drivers, month=7:9, rain_add=0.5) ##keep the lakes topped off
  driver_save(drivers)
}

to_run = unique(get_driver_index('NLDAS')$id)

#set driver location to datascience computer
clusterCall(c1, function(){library(mda.lakes);set_driver_url(driver_url)})

clusterExport(c1, 'secchi_standard')
out = clusterApplyLB(c1, to_run, future_hab_wtr, modern_era=1977:2014, 
                     driver_function=driver_fun, secchi_function=secchi_standard)

#hmm = lapply('nhd_14767902', future_hab_wtr, modern_era=1977:2012, driver_function=driver_fun, secchi_function=secchi_standard)

## 1 - temp
## 2 - core metrics
## 3 - hansen_habitat
## 4 - site_id

run_name = '2016-04-20_habitat_out'
out_dir = file.path('c:/WiLMA/habitat', run_name)
dir.create(out_dir)

good_data = out[!unlist(lapply(out, function(x){'error' %in% names(x)}))]
bad_data  = out[unlist(lapply(out, function(x){'error' %in% names(x)}))]

sprintf('%i lakes ran\n', length(good_data))
dframes = lapply(good_data, function(x){tmp = x[[1]]; tmp$site_id=x[['site_id']]; return(tmp)})
#drop the burn-in years
dframes = lapply(dframes, function(df){subset(df, DateTime > as.POSIXct('1979-01-01'))})

hansen_habitat = do.call(rbind, lapply(good_data, function(x){x[['hansen_habitat']]}))
hansen_habitat = subset(hansen_habitat, year %in% 1979:2015)

core_metrics = do.call(rbind, lapply(good_data, function(x){x[['core_metrics']]}))
core_metrics = subset(core_metrics, year %in% 1979:2015)


write.table(hansen_habitat, file.path(out_dir, 'NLDAS_best_hansen_hab.tsv'), sep='\t', row.names=FALSE)
write.table(core_metrics, file.path(out_dir, 'NLDAS_best_hansen_hab.tsv'), sep='\t', row.names=FALSE)
save('dframes', file = file.path(out_dir, 'NLDAS_best_all_wtr.Rdata'))
save('bad_data', file = file.path(out_dir, 'NLDAS_bad_data.Rdata'))

rm(out, good_data, dframes)
gc()
################################################################################
## Lets run GENMOM 1980-1999, 2020-2089
################################################################################
driver_fun = function(site_id, gcm){
  drivers = read.csv(get_driver_path(paste0(site_id, ''), driver_name = gcm, timestep = 'daily'), header=TRUE)
  nldas   = read.csv(get_driver_path(paste0(site_id, ''), driver_name = 'NLDAS'), header=TRUE)
  drivers = driver_nldas_debias_airt_sw(drivers, nldas)
  drivers = driver_add_burnin_years(drivers, nyears=2)
  drivers = driver_add_rain(drivers, month=7:9, rain_add=0.5) ##keep the lakes topped off
  driver_save(drivers)
}
clusterExport(c1, 'driver_fun')


out = clusterApplyLB(c1, to_run, future_hab_wtr, 
                     years=1978:1999, #because we added burn-in years
                     future_era=2020:2089, 
                     driver_function=function(site_id){driver_fun(site_id, 'GENMOM')})


good_data = out[!unlist(lapply(out, function(x){'error' %in% names(x)}))]
bad_data  = out[unlist(lapply(out, function(x){'error' %in% names(x)}))]

sprintf('%i lakes ran\n', length(good_data))

dframes = lapply(good_data, function(x){tmp = x[[1]]; tmp$site_id=x[[3]]; return(tmp)})
#drop the burn-in years
dframes = lapply(dframes, function(df){subset(df, DateTime > as.POSIXct('1980-01-01'))})

all_habitat = do.call(rbind, lapply(good_data, function(x){tmp = x[[2]]; tmp$site_id=x[[3]]; return(tmp)}))
all_habitat = subset(all_habitat, year %in% c(1980:1999, 2020:2089))

write.table(all_habitat, file.path(out_dir, 'GENMOM_all_habitat.tsv'), sep='\t', row.names=FALSE)
save('dframes', file = file.path(out_dir, 'GENMOM_all_wtr.Rdata'))
save('bad_data', file = file.path(out_dir, 'GENMOM_bad_data.Rdata'))

rm(out, good_data, dframes)
gc()

################################################################################
## Lets run CM2.0 1970-1999, 2040-2069
################################################################################

out = clusterApplyLB(c1, to_run, future_hab_wtr, 
                     years=1968:1999, #because we added burn-in years
                     future_era=2040:2069, 
                     driver_function=function(site_id){driver_fun(site_id, 'CM2.0')})

good_data = out[!unlist(lapply(out, function(x){'error' %in% names(x)}))]
bad_data  = out[unlist(lapply(out, function(x){'error' %in% names(x)}))]

sprintf('%i lakes ran\n', length(good_data))

all_habitat = do.call(rbind, lapply(good_data, function(x){tmp = x[[2]]; tmp$site_id=x[[3]]; return(tmp)}))
all_habitat = subset(all_habitat, year %in% c(1970:1999, 2040:2069))

write.table(all_habitat, file.path(out_dir, 'CM2.0_all_habitat.tsv'), sep='\t', row.names=FALSE)

dframes = lapply(good_data, function(x){tmp = x[[1]]; tmp$site_id=x[[3]]; return(tmp)})

#remove intermidate data structures to free up memory
rm(out, good_data)
gc()

#drop the burn-in years
#dframes = lapply(dframes, function(df){subset(df, DateTime > as.POSIXct('1970-01-01'))})

save('dframes', file = file.path(out_dir, 'CM2.0_all_wtr.Rdata'))
save('bad_data', file = file.path(out_dir, 'CM2.0_bad_data.Rdata'))

rm(dframes)
gc()

################################################################################
## Lets run ECHAM5 1969-1999, 2020-2099
################################################################################
out = clusterApplyLB(c1, to_run, future_hab_wtr, 
                     years=1967:1999, #because we added burn-in years
                     future_era=2020:2097,
                     driver_function=function(site_id){driver_fun(site_id, 'ECHAM5')})

good_data = out[!unlist(lapply(out, function(x){'error' %in% names(x)}))]
bad_data  = out[unlist(lapply(out, function(x){'error' %in% names(x)}))]

sprintf('%i lakes ran\n', length(good_data))

all_habitat = do.call(rbind, lapply(good_data, function(x){tmp = x[[2]]; tmp$site_id=x[[3]]; return(tmp)}))
all_habitat = subset(all_habitat, year %in% c(1969:1999, 2020:2097))

write.table(all_habitat, file.path(out_dir, 'ECHAM5_all_habitat.tsv'), sep='\t', row.names=FALSE)

dframes = lapply(good_data, function(x){tmp = x[[1]]; tmp$site_id=x[[3]]; return(tmp)})
rm(out, good_data)
gc()

#drop the burn-in years
dframes = lapply(dframes, function(df){subset(df, DateTime > as.POSIXct('1969-01-01'))})

save('dframes', file = file.path(out_dir, 'ECHAM5_all_wtr.Rdata'))
save('bad_data', file = file.path(out_dir, 'ECHAM5_bad_data.Rdata'))

rm(out, good_data, dframes)
gc()

