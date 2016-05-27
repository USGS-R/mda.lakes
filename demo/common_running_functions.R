
run_cal = function(site_id, years=1979:2012, driver_function=get_driver_path, secchi_function=function(site_id){}, nml_args=list()){
  
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
    
    data(wtemp)
    obs = filter(wtemp, site_id == nhd_id) %>%
      transmute(DateTime=date, Depth=depth, temp=wtemp)
    
    #having a weird issue with resample_to_field, make unique
    obs = obs[!duplicated(obs[,1:2]), ]
    
    write.table(obs, file.path(run_dir, 'obs.tsv'), sep='\t', row.names=FALSE)
    
    #get driver data
    driver_path = driver_function(site_id)
    driver_path = gsub('\\\\', '/', driver_path)
    
    
    #kds = get_kd_best(site_id, years=years, datasource = datasource)
    
    kd_avg = secchi_function(site_id) #secchi_conv/mean(kds$secchi_avg, na.rm=TRUE)
    
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
                      cd=getCD(site_id, method='Hondzo')), 
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

split_run_output = function(out){
  sprintf('%i lakes ran\n', sum(unlist(lapply(out, inherits, what='list'))))
  
  nmls = lapply(out[sapply(out, inherits, what='list')], function(x){x$nml})
  out_err = out[!unlist(lapply(out, inherits, what='list'))]
  out = lapply(out[sapply(out, inherits, what='list')], function(x){x$cal.data})
  
  ##results
  out_df = do.call('rbind', out[unlist(lapply(out, inherits, what='data.frame'))])
  
  return(list('out_df'=out_df, 'nmls'=nmls, 'out_err'=out_err))
}


secchi_target_month = function(site_id, target_month=5){
  
  nhd_id = site_id
  secchi_conv = 1.7
  kds = subset(secchi, site_id == nhd_id & source=='in-situ')
  kds$month = as.POSIXlt(kds$date)$mon + 1
  kds = subset(kds, month %in% target_month)
  
  had_month_kd = TRUE
  
  if(nrow(kds) < 1){
    #Just going to set "years" right now to full possible sim range
    kds = get_kd_best(site_id, years=1980:2014)
    kds = rename(kds, secchi_m=secchi_avg)
    had_month_kd = FALSE
  }
  
  
  kd_avg = secchi_conv/median(kds$secchi_m, na.rm=TRUE)
  return(kd_avg)
}

secchi_standard = function(site_id, datasource=NA){
  
  secchi_conv = 1.7
  kds = get_kd_best(site_id, years=1980:2015, datasource = datasource)
  
  kd_avg = secchi_conv/median(kds$secchi_avg, na.rm=TRUE)
  return(kd_avg)
}
