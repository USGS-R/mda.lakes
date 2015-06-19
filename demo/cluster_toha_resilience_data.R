## Lets cluserify things

library(parallel)

#lets try 30 to start
c1 = makePSOCKcluster(paste0('licon', 1:100), manual=TRUE, port=4042)


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

get_toha_resil = function(site_id){
  
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
    
    secchi = c(10:3, seq(2.9,0.1, by=-0.1))
    
    bare_wbic = substr(site_id, 6, nchar(site_id))
    	
  	out = data.frame()
  	
  	for(i in 1:length(secchi)){
  		prep_run_chained_glm_kd(bare_wbic, kd=1.7/secchi[i], path=run_dir, years=2007)
  		
  		out = rbind(out, chained.habitat.calc(run_dir, lakeid = bare_wbic))
  	}
  	
    unlink(run_dir, recursive=TRUE)
    
  	out$secchi = secchi
    out$site_id = site_id
      
    out
  }, error=function(e){e})
}


out = clusterApplyLB(c1, to_run, get_toha_resil)

res_toha_resil = do.call('rbind', out[unlist(lapply(out, inherits, what='data.frame'))])

to_save = res_toha_resil[, c('site_id', 'year', 'secchi', 'optic_hab_8_64', 'thermal_hab_11_25',
                             'optic_thermal_hab', 'lake_benthic_area')]


