
library(plyr)

#fname is string name
#tz.offset is timezone offset in hours (CST is -6)
load_gdp_file = function(fname, tz.offset=0){
	
	require(lubridate)
	
	## open file
	fid = file(fname, open='r')
	# drop the first line, just variable name 
	discard = readLines(fid, 1)
	# first line is lake ids (WBIC in our case)
	ids = strsplit(readLines(fid, 1), '\t', fixed=T)[[1]]
	
	#read the full data table
	data = read.table(fid, sep='\t', header=TRUE, as.is=TRUE)
	close(fid)
	
	#parse the date into POSIXct format
	data$TIMESTEP = parse_date_time2(data$TIMESTEP, 'YmdHMS') + tz.offset*60*60
	
	ids[1] = 'datetime'
	names(data) = ids
	return(data)
}

#    INPUT:   Ta - air temperature  [C]
#             Pa - (optional) pressure [mb]
#
#    OUTPUT:  q  - saturation specific humidity  [kg/kg]
qsat = function(Ta, Pa){
	ew = 6.1121*(1.0007+3.46e-6*Pa)*exp((17.502*Ta)/(240.97+Ta)) # in mb
	q  = 0.62197*(ew/(Pa-0.378*ew))                              # mb -> kg/kg
	return(q)
}

base.dir = 'D:/NLDAS'
precip 	= load_gdp_file(file.path(base.dir, 'apcpsfc.tsv'), tz.offset=-6)
dwLW 		= load_gdp_file(file.path(base.dir, 'dlwrfsfc.tsv'), tz.offset=-6)
dwSW 		= load_gdp_file(file.path(base.dir, 'dswrfsfc.tsv'), tz.offset=-6)
press 	= load_gdp_file(file.path(base.dir, 'pressfc.tsv'), tz.offset=-6)
spfHum 	= load_gdp_file(file.path(base.dir, 'spfh2m.tsv'), tz.offset=-6)
airT_k 	= load_gdp_file(file.path(base.dir, 'tmp2m.tsv'), tz.offset=-6)
uwnd 		= load_gdp_file(file.path(base.dir, 'ugrd10m.tsv'), tz.offset=-6)
vwnd 		= load_gdp_file(file.path(base.dir, 'vgrd10m.tsv'), tz.offset=-6)


kelvinConv = -273.15
out.dir = 'D:/WiLMA/MetDrivers/2014-08-14'

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
	
	fout = file.path(out.dir,paste0('WBIC_', lakeids[i], '.csv'))
	write.table(out.data, fout, sep=',', row.names=FALSE, col.names=TRUE, quote=FALSE)
	
	#append new year to data
	#write.table(format(out.data, digits=4), fout, sep=',', row.names=FALSE, 
	# col.names=FALSE, quote=FALSE,	append=TRUE)
}
	







