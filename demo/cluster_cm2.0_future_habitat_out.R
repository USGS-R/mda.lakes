## Lets cluserify things

library(parallel)

#lets try 100 to start
c1 = makePSOCKcluster(paste0('licon', 1:50), manual=TRUE, port=4043)


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

lakes = read.table(system.file('supporting_files/managed_lake_info.txt', package = 'mda.lakes'), 
                   sep='\t', quote="\"", header=TRUE, as.is=TRUE, colClasses=c(WBIC='character'))

to_run = paste0('WBIC_', lakes$WBIC)


model_future_habitat = function(site_id){
  
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
    
    driver_path = get_driver_path(paste0(site_id, '.csv'), driver_name = "GENMOM")
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
    
    out = chained.habitat.calc(run_dir, lakeid = bare_wbic)
    
    
    unlink(run_dir, recursive=TRUE)
    
    secchi = get_kd_best(site_id, years=c(modern_era, future_era))
    names(secchi) = c('year', 'secchi_avg', 'secchi_source')
    out = merge(out, secchi, by='year')
    
    out$site_id = site_id
    
    out
    
  }, error=function(e){unlink(run_dir, recursive=TRUE);e})
}


out = clusterApplyLB(c1, to_run, model_future_habitat)

all_habitat = do.call('rbind', out[unlist(lapply(out, inherits, what='data.frame'))])

#fix the gap between present and future model runs
all_habitat$`winter_dur_0-4`[all_habitat$`winter_dur_0-4` > 300] = NA

write.table(all_habitat, '~/2015-06-15_THE_FUTURE.tsv', sep='\t', row.names=FALSE)
