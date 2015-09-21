## Lets cluserify things

library(parallel)

#lets try 100 to start
c1 = makePSOCKcluster(paste0('licon', 1:50), manual=TRUE, port=4041)

# now devtools installed on cluster
#clusterCall(c1, function(){install.packages('devtools', repos='http://cran.rstudio.com')})
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


lakes = read.table(system.file('supporting_files/managed_lake_info.txt', package = 'mda.lakes'), 
                   sep='\t', quote="\"", header=TRUE, as.is=TRUE, colClasses=c(WBIC='character'))

to_run = paste0('WBIC_', lakes$WBIC)


future_hab_wtr = function(site_id, years=1979:2012, future_era, driver_function=get_driver_path){
  
  modern_era = years
  
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
    
    driver_path = driver_function(site_id)
    driver_path = gsub('\\\\', '/', driver_path)
    
    #run with different driver and ice sources
    
    prep_run_glm_kd(bare_wbic, kd=1.7/secchi$secchi_avg, path=run_dir, 
                            years=modern_era,
                            nml_args=list(
                              dt=3600, subdaily=FALSE, nsave=24, 
                              timezone=-6,
                              csv_point_nlevs=0, 
                              meteo_fl=driver_path, 
                              snow_albedo_factor=1.1))
    
    
    ##parse the habitat and WTR info. next run will clobber output.nc
    wtr_all = get_temp(file.path(run_dir, 'output.nc'), reference='surface')
    ## drop the first n burn-in years
    #years = as.POSIXlt(wtr$DateTime)$year + 1900
    #to_keep = !(years <= min(years) + nburn - 1)
    #wtr_all = wtr[to_keep, ]
    
    
    habitat = continuous.habitat.calc(run_dir, lakeid=bare_wbic)
    
    #Run future era only if requested
    if(!missing(future_era)){
      secchi = get_kd_best(site_id, years=future_era)
      
      prep_run_glm_kd(bare_wbic, kd=1.7/secchi$secchi_avg, path=run_dir, 
                              years=future_era,
                              nml_args=list(
                                dt=3600, subdaily=FALSE, nsave=24, 
                                timezone=-6,
                                csv_point_nlevs=0, 
                                meteo_fl=driver_path,
                                snow_albedo_factor=1.1))
      
      wtr = get_temp(file.path(run_dir, 'output.nc'), reference='surface', z_out = get.offsets(wtr_all))
      ## drop the first n burn-in years
      #years = as.POSIXlt(wtr$DateTime)$year + 1900
      #to_keep = !(years <= min(years) + nburn - 1)
      #wtr = wtr[to_keep, ]
      
      wtr_all = rbind(wtr_all, wtr)
      
      
      ##now hab
      habitat = rbind(habitat, continuous.habitat.calc(run_dir, lakeid=bare_wbic))
    }
    
    unlink(run_dir, recursive=TRUE)
    
    all_data = list(wtr_all, habitat, site_id)

    return(all_data)
    
  }, error=function(e){unlink(run_dir, recursive=TRUE);e})
}

################################################################################
## Lets run nldas
################################################################################
driver_fun = function(site_id){
  nldas = read.csv(get_driver_path(paste0(site_id, '.csv'), driver_name = 'NLDAS'), header=TRUE)
  drivers = driver_nldas_wind_debias(nldas)
  drivers = driver_add_burnin_years(drivers, nyears=2)
  drivers = driver_add_rain(drivers, month=7:9, rain_add=0.5) ##keep the lakes topped off
  driver_save(drivers)
}

out = clusterApplyLB(c1, to_run, future_hab_wtr, years=1977:2012, driver_function=driver_fun)

good_data = out[!unlist(lapply(out, inherits, what='simpleError'))]

sprintf('%i lakes ran\n', length(good_data))
dframes = lapply(good_data, function(x){tmp = x[[1]]; tmp$site_id=x[[3]]; return(tmp)})
#drop the burn-in years
dframes = lapply(dframes, function(df){subset(df, DateTime > as.POSIXct('1979-01-01'))})

all_habitat = do.call(rbind, lapply(good_data, function(x){tmp = x[[2]]; tmp$site_id=x[[3]]; return(tmp)}))
all_habitat = subset(all_habitat, year %in% 1979:2012)

write.table(all_habitat, 'E:/WiLMA/Results/2015-09-15_wtr_hab/NLDAS_best_all_habitat.tsv', sep='\t', row.names=FALSE)
save('dframes', file = 'E:/WiLMA/Results/2015-09-15_wtr_hab/NLDAS_best_all_wtr.Rdata')

rm(out, good_data, dframes)
gc()
################################################################################
## Lets run GENMOM 1980-1999, 2020-2089
################################################################################
driver_fun = function(site_id, gcm){
  drivers = read.csv(get_driver_path(paste0(site_id, '.csv'), driver_name = gcm), header=TRUE)
  nldas   = read.csv(get_driver_path(paste0(site_id, '.csv'), driver_name = 'NLDAS'), header=TRUE)
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


good_data = out[!unlist(lapply(out, inherits, what='simpleError'))]
sprintf('%i lakes ran\n', length(good_data))

dframes = lapply(good_data, function(x){tmp = x[[1]]; tmp$site_id=x[[3]]; return(tmp)})
#drop the burn-in years
dframes = lapply(dframes, function(df){subset(df, DateTime > as.POSIXct('1980-01-01'))})

all_habitat = do.call(rbind, lapply(good_data, function(x){tmp = x[[2]]; tmp$site_id=x[[3]]; return(tmp)}))
all_habitat = subset(all_habitat, year %in% c(1980:1999, 2020:2089))

write.table(all_habitat, 'E:/WiLMA/Results/2015-09-21_wtr_hab/GENMOM_all_habitat.tsv', sep='\t', row.names=FALSE)
save('dframes', file = 'E:/WiLMA/Results/2015-09-21_wtr_hab/GENMOM_all_wtr.Rdata')

rm(out, good_data, dframes)
gc()

################################################################################
## Lets run CM2.0 1970-1999, 2040-2069
################################################################################

out = clusterApplyLB(c1, to_run, future_hab_wtr, 
                     years=1968:1999, #because we added burn-in years
                     future_era=2040:2069, 
                     driver_function=function(site_id){driver_fun(site_id, 'CM2.0')})

good_data = out[!unlist(lapply(out, inherits, what='simpleError'))]

dframes = lapply(good_data, function(x){tmp = x[[1]]; tmp$site_id=x[[3]]; return(tmp)})
#drop the burn-in years
dframes = lapply(dframes, function(df){subset(df, DateTime > as.POSIXct('1970-01-01'))})

all_habitat = do.call(rbind, lapply(good_data, function(x){tmp = x[[2]]; tmp$site_id=x[[3]]; return(tmp)}))
all_habitat = subset(all_habitat, year %in% c(1970:1999, 2040:2069))

write.table(all_habitat, 'E:/WiLMA/Results/2015-09-21_wtr_hab/CM2.0_all_habitat.tsv', sep='\t', row.names=FALSE)
save('dframes', file = 'E:/WiLMA/Results/2015-09-21_wtr_hab/CM2.0_all_wtr.Rdata')

rm(out, good_data, dframes)
gc()

################################################################################
## Lets run ECHAM5 1969-1999, 2020-2099
################################################################################
out = clusterApplyLB(c1, to_run, future_hab_wtr, 
                     years=1967:1999, #because we added burn-in years
                     future_era=2020:2099,
                     driver_function=function(site_id){driver_fun(site_id, 'ECHAM5')})

good_data = out[!unlist(lapply(out, inherits, what='simpleError'))]

dframes = lapply(good_data, function(x){tmp = x[[1]]; tmp$site_id=x[[3]]; return(tmp)})
#drop the burn-in years
dframes = lapply(dframes, function(df){subset(df, DateTime > as.POSIXct('1969-01-01'))})

all_habitat = do.call(rbind, lapply(good_data, function(x){tmp = x[[2]]; tmp$site_id=x[[3]]; return(tmp)}))
all_habitat = subset(all_habitat, year %in% c(1969:1999, 2020:2099))

write.table(all_habitat, 'E:/WiLMA/Results/2015-09-21_wtr_hab/ECHAM5_all_habitat.tsv', sep='\t', row.names=FALSE)
save('dframes', file = 'E:/WiLMA/Results/2015-09-21_wtr_hab/ECHAM5_all_wtr.Rdata')

rm(out, good_data, dframes)
gc()

