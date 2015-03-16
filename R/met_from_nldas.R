
require(plyr)

#    INPUT:   Ta - air temperature  [C]
#             Pa - (optional) pressure [mb]
#
#    OUTPUT:  q  - saturation specific humidity  [kg/kg]
qsat = function(Ta, Pa){
	ew = 6.1121*(1.0007+3.46e-6*Pa)*exp((17.502*Ta)/(240.97+Ta)) # in mb
	q  = 0.62197*(ew/(Pa-0.378*ew))                              # mb -> kg/kg
	return(q)
}

#'@export
met_from_nldas = function(data_dir, output_dir, append_files=TRUE, overwrite=FALSE, tz.offset=0){

	if(!require(lubridate)){
		stop('lubridate must be installed for met_from* functions')
	}
	
	precip 	= load_gdp_file(file.path(data_dir, 'apcpsfc.tsv'), tz.offset=tz.offset)
	dwLW 	= load_gdp_file(file.path(data_dir, 'dlwrfsfc.tsv'), tz.offset=tz.offset)
	dwSW 	= load_gdp_file(file.path(data_dir, 'dswrfsfc.tsv'), tz.offset=tz.offset)
	press 	= load_gdp_file(file.path(data_dir, 'pressfc.tsv'), tz.offset=tz.offset)
	spfHum 	= load_gdp_file(file.path(data_dir, 'spfh2m.tsv'), tz.offset=tz.offset)
	airT_k 	= load_gdp_file(file.path(data_dir, 'tmp2m.tsv'), tz.offset=tz.offset)
	uwnd 	= load_gdp_file(file.path(data_dir, 'ugrd10m.tsv'), tz.offset=tz.offset)
	vwnd 	= load_gdp_file(file.path(data_dir, 'vgrd10m.tsv'), tz.offset=tz.offset)


	kelvinConv = -273.15

	lakeids = names(precip)[-1]

	for(i in 1:length(lakeids)){
		
		data = precip[,c('datetime',lakeids[i])]
		data = merge(data, dwLW[,c('datetime',lakeids[i])], by='datetime')
		data = merge(data, dwSW[,c('datetime',lakeids[i])], by='datetime')
		data = merge(data, press[,c('datetime',lakeids[i])], by='datetime')
		data = merge(data, spfHum[,c('datetime',lakeids[i])], by='datetime')
		data = merge(data, airT_k[,c('datetime',lakeids[i])], by='datetime')
		data = merge(data, uwnd[,c('datetime',lakeids[i])], by='datetime')
		data = merge(data, vwnd[,c('datetime',lakeids[i])], by='datetime')
		
		
		names(data) = c('datetime','precip','dwLW','dwSW','press','spfHum','airT_k','uwnd','vwnd')
		
		headers = 'time,ShortWave,LongWave,AirTemp,RelHum,WindSpeed,Rain,Snow\n';
		
		data$time = as.POSIXct(trunc(data$datetime, units='days'))
		out.data = data.frame(time=unique(data$time))
		
		## drop any days with less than 24 observations
		data = ddply(data, 'time', function(df){
			if(nrow(df) >=24){
				return(df)
			}else{
				return(data.frame())
			}
			
		})
		
		## convert and downsample wind
		out.data = merge(out.data, ddply(data[,c('time','uwnd', 'vwnd')], 'time', function(df){
			
			tmp = sqrt(df$uwnd^2 + df$vwnd^2) #just need non-directional speed
			tmp = mean(tmp^3)^(1/3) 		      #use power averaging
			
			return(data.frame('WindSpeed'=tmp))
		}))
		
		out.data = merge(out.data, ddply(data[,c('time','dwSW')], 'time', function(df){
			return(data.frame('ShortWave'=mean(df$dwSW)))
		}))
		
		out.data = merge(out.data, ddply(data[,c('time','dwLW')], 'time', function(df){
			return(data.frame('LongWave'=mean(df$dwLW)))
		}))
		
		out.data = merge(out.data, ddply(data[,c('time','airT_k')], 'time', function(df){
			return(data.frame('AirTemp'=mean(df$airT_k - 273.15))) #convert to C
		}))
		
		out.data = merge(out.data, ddply(data[,c('time','precip')], 'time', function(df){
			return(data.frame('Rain'=mean(df$precip*24/1000)))
		}))
		
		out.data = merge(out.data, ddply(data[,c('time','press', 'spfHum', 'airT_k')], 'time', function(df){
			sat_hum = qsat(df$airT_k-273.15, df$press*0.01)
			rh = 100*df$spfHum/sat_hum
			return(data.frame(RelHum=mean(rh)))
			
		}))
		
		out.data$Snow = 0
		
		out.data = out.data[order(out.data$time), c('time','ShortWave','LongWave','AirTemp','RelHum','WindSpeed','Rain','Snow')]
		
		#Format time as string in the correct way
		out.data$time = format(out.data$time,'%Y-%m-%d %H:%M:%S')
		
		fout = file.path(out_dir,paste0('WBIC_', lakeids[i], '.csv'))
		
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





