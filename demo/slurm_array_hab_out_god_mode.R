#slurm_array_hab_out_god_mode
## This uses SLURM array running to run individual lakes
## MPI has been too troublesome

library(lakeattributes)
library(mda.lakes)

################################################################################
### read in env var config
#This should be 1 to n, where n may be larger than total number of lakes to model
task_id = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID', 'NA'))

if(is.na(task_id)){
  stop("ERROR Can not read task_id, NA returned")
}

task_offset = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID_offset', 'NA'))
if(is.na(task_offset)){
  task_offset = 0
}
task_id = task_id + task_offset

################################################################################
### read in config file config
config = read.table('config', header=TRUE, as.is=TRUE)

driver_name = config$drivername
driver_url = config$driverurl
out_dir = file.path(config$outdir, driver_name)
set_driver_url(driver_url)

to_run = as.character(unique(zmax$site_id))
if(task_id > length(to_run)){
  sprintf('Skipping task_id:%i because greater than number of lakes to run')
  q(save='no', status=0)
}

site_id = to_run[task_id]


run_necsc_lake(site_id, driver_name, out_dir)

