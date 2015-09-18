## Lets cluserify things

library(parallel)

#Build cluster
c1 = makePSOCKcluster(paste0('licon', 1:50), manual=TRUE, port=4043)
#c1 = makePSOCKcluster('localhost')

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

model_habitat = function(site_id){
  
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
    
    secchi = get_kd_best(site_id, years=1979:2012)
    
    bare_wbic = substr(site_id, 6, nchar(site_id))
  	
    
  	prep_run_chained_glm_kd(bare_wbic, 
                            kd=1.7/secchi$secchi_avg, path=run_dir, 
                            years=1979:2012,
  	                        nml_args=list(
  	                          dt=3600, subdaily=FALSE, nsave=24, 
  	                          timezone=-6,
  	                          csv_point_nlevs=0)
                            )
  	
  	out = chained.habitat.calc(run_dir, lakeid = bare_wbic)
  	
    unlink(run_dir, recursive=TRUE)
  
    names(secchi) = c('year', 'secchi_avg', 'secchi_source')
  	out = merge(out, secchi, by='year')
  
    out$site_id = site_id
      
    out
  
  }, error=function(e){unlink(run_dir, recursive=TRUE);e})
}


out = clusterApplyLB(c1, to_run, model_habitat)

all_habitat = do.call('rbind', out[unlist(lapply(out, inherits, what='data.frame'))])

write.table(all_habitat, '~/2015-06-24_gretchen_secchi_hierarchy.tsv', sep='\t', row.names=FALSE)

