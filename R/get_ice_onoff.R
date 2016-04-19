#'
#'@title Get Ice On/Off Dates
#'@description Function to estimate the ice on/off dates from a timeseries of water temperature
#'and ice data. 
#'
#'@param wtr data.frame of water temp data, from \code{get_temp} function
#'@param ice data.frame of ice data, from \code{get_ice} function
#'
#'@return data.frame with columns \code{c('year', 'on', 'off')}
#'
#'
#'
#'
#'@importFrom accelerometry rle2
#'
#'@export
get_ice_onoff = function(ice, wtr){	
	
	peak_temps = get_annual_temp_peaks(wtr)
	
	peak_temps = peak_temps[complete.cases(peak_temps), ]
	
	ice_onoff = data.frame(year=peak_temps$year, on=as.POSIXct(NA), off=as.POSIXct(NA))
	
	for(i in 1:nrow(peak_temps)){
		#find overall index matching peak_temp date
		peak_date = peak_temps$max_date[i]
		peak_minus_365 = peak_date - as.difftime(365, units="days")
		peak_plus_365 = peak_date + as.difftime(365, units="days")
		
		##get 365 days of ice after peak_temp
		#find ice-on
		after_ice = ice[ice$DateTime >= peak_date & ice$DateTime <= peak_plus_365, ]$`ice(m)`
		
		ice_runs = rle2(as.numeric(after_ice > 0), indices=TRUE)
		
		#ice on is encoded as a run value of 1
		ice_only = ice_runs[ice_runs[,'values'] == 1, ,drop=FALSE]
		
		if(nrow(ice_only) == 0){
			ice_onoff$on[i] = NA
		}else{
			#grab the longest ice run to use for on/off dates 
			longest_ice = ice_only[which.max(ice_only[,'lengths']), , drop=FALSE]
			
			ice_onoff$on[i] = ice[ice$DateTime >= peak_date & ice$DateTime <= peak_plus_365, ]$DateTime[longest_ice[,'starts']]
		}
		
		
		##get 365 days of ice before peak_temp and reverse
		#find ice-off
		before_ice = ice[ice$DateTime >= peak_minus_365 & ice$DateTime <= peak_date, ]$`ice(m)`
		
		ice_runs = rle2(as.numeric(before_ice > 0), indices=TRUE)
		
		#ice on is encoded as a run value of 1
		ice_only = ice_runs[ice_runs[,'values'] == 1, ,drop=FALSE]
		
		if(nrow(ice_only) == 0){
			ice_onoff$off[i] = NA
		}else{
			longest_ice = ice_only[which.max(ice_only[,'lengths']), , drop=FALSE]
			
			ice_onoff$off[i] = ice[ice$DateTime >= peak_minus_365 & ice$DateTime <= peak_date, ]$DateTime[longest_ice[,'stops']]
		}
	}
	# add this to indicate information origin
	ice_onoff$icedate_origin = 'ice'
	
	return(ice_onoff)
}

