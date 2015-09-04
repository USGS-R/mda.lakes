## Lets cluserify things

library(parallel)

#Build cluster
#c1 = makePSOCKcluster(paste0('licon', 1:50), manual=TRUE, port=4043)
c1 = makePSOCKcluster(rep('localhost', 1))

clusterCall(c1, function(){install.packages('devtools', repos='http://cran.rstudio.com')})
clusterCall(c1, function(){install.packages('rLakeAnalyzer', repos='http://cran.rstudio.com')})
clusterCall(c1, function(){install.packages('dplyr', repos='http://cran.rstudio.com')})


clusterCall(c1, function(){library(devtools)})

glmr_install     = clusterCall(c1, function(){install_github('lawinslow/GLMr')})
glmtools_install = clusterCall(c1, function(){install_github('lawinslow/glmtools')})
lakeattr_install = clusterCall(c1, function(){install_github('lawinslow/lakeattributes')})
mdalakes_install = clusterCall(c1, function(){install_github('lawinslow/mda.lakes')})

stopCluster(c1)
c1 = makePSOCKcluster(rep('localhost', 6))

#library(lakeattributes)
#library(mda.lakes)
#library(dplyr)
#library(glmtools)

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



lakes = read.table(system.file('supporting_files/managed_lake_info.txt', package = 'mda.lakes'), 
                   sep='\t', quote="\"", header=TRUE, as.is=TRUE, colClasses=c(WBIC='character'))

to_run = paste0('WBIC_', lakes$WBIC)

model_habitat = function(site_id, secchi_trend=0){
  
  library(lakeattributes)
  library(mda.lakes)
  library(dplyr)
  library(glmtools)
  library(stringr)
  
  tryCatch({
    fastdir = tempdir()
    if(file.exists('/mnt/ramdisk')){
      fastdir = '/mnt/ramdisk'
    }else if(file.exists('F:/')){
      fastdir = 'F:'
    }
    
    run_dir = file.path(fastdir, site_id)
    dir.create(run_dir)
    
    bare_wbic = substr(site_id, 6, nchar(site_id))
    
    driver_path = tempfile(fileext='.csv')
    nldas_wind_debias(get_driver_path(paste0(site_id, '.csv'), 'NLDAS'),
                      dbiased_path=driver_path)
    
    driver_path = gsub('\\\\', '/', driver_path)
    
    vals = prep_run_chained_glm_secchi(bare_wbic, 
                                     path = run_dir,
                                     start = as.POSIXct('1979-01-01'), 
                                     end = as.POSIXct('2012-12-30'),
                                     secchi_trend = secchi_trend,
                                     nml_args=list(
                                       dt=3600, subdaily=FALSE, nsave=24, 
                                       timezone=-6,
                                       csv_point_nlevs=0, 
                                       meteo_fl=driver_path))
    
    out = chained.habitat.calc.kevin(run_dir, lakeid=bare_wbic)
    
    unlink(run_dir, recursive=TRUE)
    
    #names(secchi) = c('year', 'secchi_avg', 'secchi_source')
    out = merge(out, vals, by='year')
    
    #out$site_id = site_id
    
    out
    
  }, error=function(e){unlink(run_dir, recursive=TRUE);e})
}


#-0.92
#0.92
#0

out = clusterApplyLB(c1, to_run, model_habitat, secchi_trend=0.92)

increasing = do.call('rbind', out[unlist(lapply(out, inherits, what='data.frame'))])
write.table(increasing, '~/2015-07-12_kevin_increasing.tsv', sep='\t', row.names=FALSE)

out = clusterApplyLB(c1, to_run, model_habitat, secchi_trend=-0.92)

decreasing = do.call('rbind', out[unlist(lapply(out, inherits, what='data.frame'))])
write.table(decreasing, '~/2015-07-12_kevin_decreasing.tsv', sep='\t', row.names=FALSE)

out = clusterApplyLB(c1, to_run, model_habitat, secchi_trend=0)

stable = do.call('rbind', out[unlist(lapply(out, inherits, what='data.frame'))])
write.table(stable, '~/2015-07-12_kevin_stable.tsv', sep='\t', row.names=FALSE)


