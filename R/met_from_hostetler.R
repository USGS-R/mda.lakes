
################################################################################

#'@importFrom lubridate parse_date_time2
#'@importFrom plyr ddply
#'@export
met_from_hostetler = function(data_dir, output_dir, append_files=FALSE, overwrite=TRUE, tz.offset=0){
	
	precip 	= load_gdp_file(file.path(data_dir, 'RT'), tz.offset=tz.offset)
	dwLW 	= load_gdp_file(file.path(data_dir, 'LWD'), tz.offset=tz.offset)
	dwSW 	= load_gdp_file(file.path(data_dir, 'SWI'), tz.offset=tz.offset)
	relHum 	= load_gdp_file(file.path(data_dir, 'RHA'), tz.offset=tz.offset)
	airT_C 	= load_gdp_file(file.path(data_dir, 'TA'), tz.offset=tz.offset)
	uwnd 	= load_gdp_file(file.path(data_dir, 'UA'), tz.offset=tz.offset)
	vwnd 	= load_gdp_file(file.path(data_dir, 'VA'), tz.offset=tz.offset)
	
	
	lakeids = names(precip)[-1]
	
	for(i in 1:length(lakeids)){
		
		#This order is important
		data = precip[,c('datetime',lakeids[i])]
		data = merge(data, dwLW[,c('datetime',lakeids[i])], by='datetime')
		data = merge(data, dwSW[,c('datetime',lakeids[i])], by='datetime')
		data = merge(data, relHum[,c('datetime',lakeids[i])], by='datetime')
		data = merge(data, airT_C[,c('datetime',lakeids[i])], by='datetime')
		data = merge(data, uwnd[,c('datetime',lakeids[i])], by='datetime')
		data = merge(data, vwnd[,c('datetime',lakeids[i])], by='datetime')
		
		
		names(data) = c('datetime','precip','LongWave','ShortWave','RelHum','AirTemp','uwnd','vwnd')
		
		headers = 'time,ShortWave,LongWave,AirTemp,RelHum,WindSpeed,Rain,Snow\n';
		
		data$time = data$datetime
		out.data = data.frame(time=unique(data$time))
		
		## convert and downsample wind
		out.data = merge(out.data, ddply(data[,c('time','uwnd', 'vwnd')], 'time', function(df){
			
			tmp = sqrt(df$uwnd^2 + df$vwnd^2) #just need non-directional speed
			tmp = mean(tmp^3)^(1/3) 		      #use power averaging
			
			return(data.frame('WindSpeed'=tmp))
		}))
		
		out.data = merge(out.data, data[,c('time','ShortWave')])
		
		out.data = merge(out.data, data[,c('time','LongWave')])
		
		out.data = merge(out.data, data[,c('time','AirTemp')])
		
		out.data = merge(out.data, data[,c('time', 'RelHum')])
		out.data$RelHum = out.data$RelHum*100 #convert to percent
		
		#precip/rain/snow
		out.data = merge(out.data, data[,c('time','precip')])
		out.data$precip = out.data$precip/1000 #convert rate from mm/day to m/day
		
		out.data$precip[out.data$precip < 1e-5] = 0
		
		names(out.data)[names(out.data) %in% 'precip'] = 'Rain'
		
		out.data$Snow = 0 
		
		# 10:1 ratio assuming 1:10 density ratio water weight
		out.data$Snow[out.data$AirTemp < 0] = out.data$Rain[out.data$AirTemp < 0]*10 
		out.data$Rain[out.data$AirTemp < 0] = 0
		
		out.data = out.data[order(out.data$time), c('time','ShortWave','LongWave','AirTemp','RelHum','WindSpeed','Rain','Snow')]
		
		#Format time as string in the correct way
		out.data$time = format(out.data$time,'%Y-%m-%d %H:%M:%S')
		
		fout = file.path(output_dir, paste0('WBIC_', lakeids[i], '.csv'))
		
		if(append_files){
			#append new year to data
			write.table(format(out.data, digits=4), fout, sep=',', row.names=FALSE, 
									col.names=FALSE, quote=FALSE,	append=TRUE)
		}else{
			if(file.exists(fout) & !overwrite){
				stop(fout, ' exists and overwrite is set to FALSE')
			}
			write.table(out.data, fout, sep=',', row.names=FALSE, col.names=TRUE, quote=FALSE)
		}
		
	}#lakes for loop
}#function close	
