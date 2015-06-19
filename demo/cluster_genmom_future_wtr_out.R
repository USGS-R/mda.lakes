## Lets cluserify things

library(parallel)

#lets try 100 to start
c1 = makePSOCKcluster(paste0('licon', 1:50), manual=TRUE, port=4042)


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
library(reshape2)

lakes = read.table(system.file('supporting_files/managed_lake_info.txt', package = 'mda.lakes'), 
                   sep='\t', quote="\"", header=TRUE, as.is=TRUE, colClasses=c(WBIC='character'))

to_run = paste0('WBIC_', lakes$WBIC)


future_wtr_out = function(site_id){
  
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
    
    secchi = get_kd_best(site_id, years=modern_era)
    
    bare_wbic = substr(site_id, 6, nchar(site_id))
    
    driver_path = get_driver_path(paste0(site_id, '.csv'), 'GENMOM')
    driver_path = gsub('\\\\', '/', driver_path)
    
    #run with different driver and ice sources
    
    prep_run_chained_glm_kd(bare_wbic, kd=1.7/secchi$secchi_avg, path=run_dir, years=modern_era,
                            ice_src='empirical.cm2.0.ice.tsv',
                            nml_args=list(
                              dt=3600, subdaily=FALSE, nsave=24, 
                              timezone=-6,
                              csv_point_nlevs=0, 
                              meteo_fl=driver_path))
    
    
    secchi = get_kd_best(site_id, years=future_era)
    
    prep_run_chained_glm_kd(bare_wbic, kd=1.7/secchi$secchi_avg, path=run_dir, years=future_era,
                            ice_src='empirical.cm2.0.ice.tsv',
                            nml_args=list(
                              dt=3600, subdaily=FALSE, nsave=24, 
                              timezone=-6,
                              csv_point_nlevs=0, 
                              meteo_fl=driver_path))
    
    
    sims = Sys.glob(file.path(run_dir, 'output*.nc'))
    
    depths = get.offsets(get_temp(sims[1], reference = 'surface'))
    
    #loop over years
    all.data = data.frame()
    for(i in 1:length(sims)){
      tmp = get_temp(sims[i], reference='surface', z_out=depths)
      all.data = rbind(all.data, tmp)
    }
    
    unlink(run_dir, recursive=TRUE)
    
    all.data$site_id = site_id
    
    all.data
    
  }, error=function(e){unlink(run_dir, recursive=TRUE);e})
}


out = clusterApplyLB(c1, to_run, future_wtr_out)

out_path = '~/FUTURE_GENMOM'
dir.create(out_path)
  
for(i in 1:length(dframes)){
  
  cat(i, '\n')  
  d = dframes[[i]]
  site_id = d$site_id[1]
  d$site_id = NULL
  tmp = gzfile(paste0(out_path, '/', site_id, '.tsv.gz'))
  write.table(d, tmp , sep='\t', row.names=FALSE, quote=FALSE)
}




