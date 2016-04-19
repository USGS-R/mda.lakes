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
