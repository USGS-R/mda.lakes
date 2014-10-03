#'
#'@title Get full WTR data frame for chained run
#'
#'
#'
#'@import rLakeAnalyzer
#'@import glmtools
#'@export
get_wtr_chained = function(run.dir){
	
	nc.files = Sys.glob(file.path(run.dir, 'output*.nc'))
	
	
	## Subsample run to get water temp data at 4D obs points
	wtr = get_temp(nc.files[1], reference='surface')
	z_outs = get.offsets(wtr[,-1])
	
	
	for(i in 2:length(nc.files)){
		tryCatch({
			tmp = get_temp(nc.files[i], reference="surface", z_out=z_outs)
			
			wtr = rbind(wtr, tmp) 
			
		}, error=function(e){warning(e)}) #If we error, just skip this year
	}
	

	return(wtr)
	
}
