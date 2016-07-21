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
mdalakes_install = clusterCall(c1, function(){install_url(paste0('http://', local_url,'/mda.lakes_3.0.4.tar.gz'))})

library(lakeattributes)
library(mda.lakes)
library(dplyr)
library(glmtools)

## setup run function
source('demo/common_running_functions.R')


driver_fun = function(site_id){
  nldas = read.csv(get_driver_path(site_id, driver_name = 'NLDAS', timestep = 'daily'), header=TRUE)
  drivers = driver_nldas_wind_debias(nldas)
  drivers = driver_add_burnin_years(drivers, nyears=2)
  drivers = driver_add_rain(drivers, month=7:9, rain_add=0.5) ##keep the lakes topped off
  driver_save(drivers)
}


#we want only ramdisk enabled nodes
#ramdisk = clusterCall(c1, function(){file.exists('/mnt/ramdisk')})
#c1 = c1[unlist(ramdisk)]
data(wtemp)
to_run = unique(intersect(intersect(wtemp$site_id, zmax$site_id), secchi$site_id))
#to_run = to_run[to_run %in% read.table('~/managed_lakes_wilma_nhd.tsv', sep='\t', header=TRUE, as.is=TRUE)$id]

run_name = '2016-04-12_cal_2014stop_NLDAS'
run_comment = "standard, all Secchi 1980-2014 run to compare to GCMs"

#clusterCall(c1, function(){library(mda.lakes);set_driver_url(driver_url)})

out = clusterApplyLB(c1, to_run, run_cal, driver_function = driver_fun, nml_args=list(), 
                     years=1980:2014, secchi_function=secchi_function=function(site_id){secchi_target_month(site_id, target_month=5:6)})

outl = split_run_output(out)
out_err = outl[['out_err']]
nml     = outl[['nmls']]

##results
write.table(outl[['out_df']], paste0('c:/WiLMA/results/', run_name, '.tsv'), sep='\t', row.names=FALSE)

save(out_err, file = paste0('c:/WiLMA/results/', run_name, '.Rdata'))

save(nmls, file=paste0('c:/WiLMA/results/', run_name, 'nml.Rdata'))

rmarkdown::render('demo/document_calibration_overview_template.Rmd', output_file=paste0(run_name,'.html'),
                  output_dir='c:/WiLMA/results/', params=list(out_df=out_df, run_message=run_comment))


sqrt(mean((out_df$Observed_temp - out_df$Modeled_temp)^2, na.rm=TRUE))
mean(out_df$Observed_temp - out_df$Modeled_temp, na.rm=TRUE)

parent = '570e8688e4b0ef3b7ca24d8e'
files = c(Sys.glob(paste0('c:/WiLMA/results/', run_name, '.*')), Sys.glob(paste0('c:/WiLMA/results/', run_name, 'nml.*')))

library(sbtools)
authenticate_sb('lwinslow@usgs.gov', pass)
itm = item_create(parent, title=run_name)

item_append_files(itm, files)




################################################################################
## ACCESS
################################################################################
driver_fun = function(site_id){
  drivers = read.csv(get_driver_path(site_id, driver_name = 'ACCESS', timestep = 'daily'), header=TRUE)
  #nldas   = read.csv(get_driver_path(site_id, driver_name = 'NLDAS', timestep = 'daily'), header=TRUE)
  #drivers = driver_nldas_debias_airt_sw(drivers, nldas)
  drivers = driver_add_burnin_years(drivers, nyears=2)
  drivers = driver_add_rain(drivers, month=7:9, rain_add=0.5) ##keep the lakes topped off
  driver_save(drivers)
}

run_name = '2016-03-28_ACCESS_WILMALakes'
run_comment = "Look at bias and RMSE for Notaro ACCESS drivers on WiLMA only lakes. Years are only 1980:1999"

clusterCall(c1, function(){library(mda.lakes);set_driver_url('http://cida-test.er.usgs.gov/mda.lakes/')})

clusterExport(c1, 'driver_fun')

out = clusterApplyLB(c1, to_run, run_cal, years=1980:1999, driver_function=driver_fun, datasource=NA)


sprintf('%i lakes ran\n', sum(unlist(lapply(out, inherits, what='data.frame'))))


##results
out_df = do.call('rbind', out[unlist(lapply(out, inherits, what='data.frame'))])
write.table(out_df, paste0('c:/WiLMA/results/', run_name, '.tsv'), sep='\t', row.names=FALSE)

rmarkdown::render('demo/document_calibration_overview_template.Rmd', output_file=paste0(run_name,'.pdf'), 
                  output_dir='c:/WiLMA/results/', params=list(out_df=out_df, run_message=run_comment))


sqrt(mean((out_df$Observed_temp - out_df$Modeled_temp)^2, na.rm=TRUE))
mean(out_df$Observed_temp - out_df$Modeled_temp, na.rm=TRUE)


# groups = split(to_run, ceiling(seq_along(to_run)/100))
# out = list()
# for(grp in groups){
#   tmp = clusterApplyLB(c1, grp, run_cal, years=1980:1999, driver_function=function(site_id){driver_fun(site_id, 'GENMOM')})
#   out = c(out, tmp)
#   cat('iteration\n')
# }


