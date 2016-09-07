### Cleanup missing run lakes

library(lakeattributes)
library(mda.lakes)

################################################################################
### read in config file config
config = read.table('config', header=TRUE, as.is=TRUE)

driver_name = config$drivername
driver_url = config$driverurl
out_dir = file.path(config$outdir, driver_name)
set_driver_url(driver_url)

rundirs = Sys.glob(file.path(out_dir, '*'))

for(i in 1:length(rundirs)){
  #NLDAS will have at least 5 outfiles
  if(length(dir(rundirs[i])) > 4){
    next
  }
  
  site_id = basename(rundirs[i])
  cat('Running ', site_id, '...\n')
  
  run_necsc_lake(site_id, driver_name, out_dir)
}




