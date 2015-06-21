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
#'
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
		
		tmp_indx = which(after_ice > 0)
		if(length(tmp_indx) == 0){
			ice_onoff$on[i] = NA
		}else{
			ice_onoff$on[i] = ice[ice$DateTime >= peak_date & ice$DateTime <= peak_plus_365, ]$DateTime[min(tmp_indx)]
		}
		
		
		##get 365 days of ice before peak_temp and reverse
		#find ice-off
		before_ice = ice[ice$DateTime >= peak_minus_365 & ice$DateTime <= peak_date, ]$`ice(m)`
		
		tmp_indx = which(before_ice > 0)
		if(length(tmp_indx) == 0){
			ice_onoff$off[i] = NA
		}else{
			ice_onoff$off[i] = ice[ice$DateTime >= peak_minus_365 & ice$DateTime <= peak_date, ]$DateTime[max(tmp_indx)]
		}
	}
	
	return(ice_onoff)
}


get_annual_temp_peaks = function(wtr){
	
	temp_max = apply(wtr[,-1], 1, max, na.rm=TRUE)
	years = as.POSIXlt(wtr$DateTime)$year + 1900
	uyears = unique(years)
	
	output = data.frame(year=uyears, max_date=as.POSIXct(Sys.Date()), max_val=NA)
	
	for(i in 1:nrow(output)){
		
		if(diff(range(wtr$DateTime[years==output$year[i]])) < as.difftime(364,units="days")){
			output$max_date[i] = NA
			output$max_val[i]  = NA
			next
		}
		
		max_indx = which.max(temp_max[years==output$year[i]])
		
		output$max_date[i] = wtr$DateTime[years==output$year[i]][max_indx]
		output$max_val[i]  = temp_max[years==output$year[i]][max_indx]
	}
	
	return(output)
}