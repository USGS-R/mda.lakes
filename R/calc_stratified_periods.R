#' @title Calculate all stratified periods based on temp threshold
#' 
#' @param surfT data.frame with two columns, datetime and surface temperature
#' @param botT data.frame with two columns, datetime and bottom temperature (must be same nrow as surfT)
#' @param temp_thresh Threshold to define stratified in deg C (default = 1)
#' @param force_positive Is only warm stratified periods to be considered (drop winter when 0 is above 4 deg water, default TRUE)
#' 
#' @description 
#' A function to calculate 
#' 
#' @importFrom accelerometry rle2
#' 
#' @export
calc_stratified_periods = function(surfT, botT, temp_thresh = 1, force_positive = TRUE){
	
	
	##assume they are data.frames to start
	datetime = surfT[,1]
	surfT = surfT[,2]
	botT = botT[,2]
	
	
	if(length(botT) != length(surfT)){
		stop('botT and surfT must be the same length or number of rows')
	}
	
	t_diff = surfT - botT
	if(force_positive){
		t_diff[t_diff < 0] = 0 #force only warm stratification
	}else{
		t_diff = abs(t_diff) #use both warm and cold stratification
	}
	
	
	periods = rle2(as.numeric(t_diff >= temp_thresh), indices = TRUE)
	
	strat_periods = as.data.frame(periods[periods[,'values'] == 1, ,drop=FALSE])
	strat_periods$values = NULL
	
	#if no strat periods were found, just return empty data.frame
	if(nrow(strat_periods) == 0){
		return(strat_periods)
	}
	
	strat_periods$onset = datetime[strat_periods$starts]
	strat_periods$breakdown = datetime[strat_periods$stops]
	
	return(strat_periods)
}