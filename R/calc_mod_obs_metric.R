#' @title Calculate limno metrics for lots of lakes
#' 
#' @description 
#' Calculte aggregate metrics from a data frame with modeled and observed data. 
#' Metric options include all available rLakeAnalyzer metrics
#' 
#' @seealso \code{\link[glmtools]{compare_to_field}}
#' 
#' 
#' 
#' @export
calc_mod_obs_metric = function(mod_obs_df, metric){
	
	
	#for each lake
	uids = unique(mod_obs_df$site_id)
	
	all_out = data.frame()
	
	for(lid in uids){
		lake_data = subset(mod_obs_df, site_id == lid)
		lake_data$site_id = NULL
		bathy = get_bathy(lid)
		
		lake_data = ddply(lake_data, 'DateTime', function(df){
									if(nrow(na.omit(df)) >= 5){
										return(na.omit(df))
									}else{
										return(data.frame())
									}
								})
		
		#run compare to field
		calc_metric = glmtools:::.compare_to_field(lake_data, bthA=bathy$areas, bthD=bathy$depths, metric=metric, as_value=TRUE)
		
		if(nrow(calc_metric) > 0){
			calc_metric$site_id = lid
			#merge metrics together
			all_out = rbind(all_out, calc_metric)
		}
		
	}
	
	#output
	return(all_out)
	
}