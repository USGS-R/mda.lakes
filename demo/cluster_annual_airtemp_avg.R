
## Lets cluserify things

library(parallel)

#lets try 30 to start
c1 = makePSOCKcluster(paste0('licon', 1:50), manual=TRUE, port=4042)


clusterCall(c1, function(){install.packages('devtools', repos='http://cran.rstudio.com')})
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

lakes = read.table(system.file('supporting_files/managed_lake_info.txt', package = 'mda.lakes'), 
                   sep='\t', quote="\"", header=TRUE, as.is=TRUE, colClasses=c(WBIC='character'))

to_run = paste0('WBIC_', lakes$WBIC)


omg_this = function(site_id){
  library(lubridate)
  library(mda.lakes)
  tryCatch({
    
    bare_wbic = substr(site_id, 6, nchar(site_id))
    fname = get_driver_path(paste0(site_id, '.csv'))
    
    all_data = read.csv(fname, header=TRUE)
    
    all_data$time = as.POSIXct(all_data$time)
    all_data$year = year(all_data$time)
    all_data$month = month(all_data$time)
    
    year_average = group_by(all_data, year) %>% summarise(temp_avg = mean(AirTemp))
    
    year_average
    
  }, error=function(e){e})

}

argh = clusterApplyLB(c1, to_run, omg_this)

res_argh = do.call('rbind', argh[unlist(lapply(argh, inherits, what='data.frame'))])

all_avg = as.data.frame(group_by(res_argh, year) %>%
                        summarise(avg = mean(temp_avg))) #%>%
                        #filter(year > 2002, year <= 2012)

all_avg[order(all_avg$avg),]


