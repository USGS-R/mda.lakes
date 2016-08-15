library(mda.lakes)


config = read.table('config', header=TRUE, as.is=TRUE)

driver_name = config$drivername
driver_url = config$driverurl
out_dir = config$outdir
scratch_dir = config$scratch

#scratch_dir = '/cxfs/projects/usgs/water/owi/lwinslow/scratch/'
dir.create(scratch_dir)

combine_output_data(driver_name, out_dir, fast_tmp=scratch_dir)

# #fast temp location for bundling wtr out data
# #this needs about 60GB of scratch space available
# #if ram_scratch is there, use it
# fast_tmp = Sys.getenv('RAM_SCRATCH', unset = '')
# 
# if(fast_tmp == ''){
#   if(file.exists('/cxfs/scratch')){
#     fast_tmp = '/cxfs/scratch'
#   }else{
#     fast_tmp = tempdir()
#   }
# }



