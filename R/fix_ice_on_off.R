#'@title Fix ice on/off for no-ice
#'
#'@description
#'Calculating metrics on lakes that sometimes, but do not consistently freeze
#'is a bear, especially with future projections which forecast more frequent
#'no-freeze years. This function fills in missing ice on/off dates with 
#'the date of minimum average lake temp. Seems to be a reasonable enough 
#'choice. There is no great one.
#'
#'@param on_off data.frame of ice on-off dates from \link{get_ice_onoff}.
#'@param wtr Water temperature data.frame from \link[glmtools]{get_temp}
#'@param hypso Hypsography data.frame in rLakeAnalyzer format \link[rLakeAnalyzer]{load.bathy}
#'
#'
#'
#'@export
fix_ice_on_off = function(on_off, wtr, hypso){
	
	#for all NA values, we need to find the minimum temperature as a replacement
	# so we can still delinate meaningful seasons
	
	mins = get_annual_temp_mins(wtr, hypso)
	
	for(i in 1:nrow(on_off)){
		
		if(is.na(on_off$on[i])){
			to_use = subset(mins, year == (on_off$year[i]))
			on_off$on[i] = to_use$min_date
			on_off$icedate_origin[i] = 'min_temp'
		}
		
		if(is.na(on_off$off[i])){
			to_use = subset(mins, year == (on_off$year[i]-1))
			on_off$off[i] = to_use$min_date
			on_off$icedate_origin[i] = 'min_temp'
		}
	}
	
	return(on_off)
}

get_annual_temp_mins = function(wtr, hypso){
	
	la.wtr = wtr
	names(la.wtr) = tolower(names(la.wtr))
	tmp_calc = ts.layer.temperature(la.wtr, 0, max(get.offsets(la.wtr)), hypso, na.rm=TRUE)
	
  temp_avg = tmp_calc[,-1]#apply(wtr[,-1], 1, min, na.rm=TRUE)
  datetime = tmp_calc[,1]
	years = as.POSIXlt(wtr$DateTime)$year + 1900
	uyears = unique(years)
	
	output = data.frame(year=uyears, min_date=as.POSIXct(Sys.Date()), min_val=NA)
	
	for(i in 1:nrow(output)){
		
		season_indx = datetime >= as.POSIXct(paste0(output$year[i], '-10-01')) & datetime <= as.POSIXct(paste0(output$year[i]+1, '-04-01'))
		
		min_indx = which.min(temp_avg[season_indx])
		
		if(length(min_indx)< 1){
			output$min_date[i] = NA
			output$min_val[i]  = NA
		}else{
			output$min_date[i] = wtr$DateTime[season_indx][min_indx]
			output$min_val[i]  = temp_avg[season_indx][min_indx]
		}
	}
	
	return(output)
}
