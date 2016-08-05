library(mda.lakes)


config = read.table('config', header=TRUE, as.is=TRUE)

driver_name = config$drivername
driver_url = config$driverurl
out_dir = file.path(config$outdir, driver_name)


combine_output_data(driver_name, out_dir)

