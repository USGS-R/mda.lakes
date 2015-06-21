
#'@title Estimate a full irradiance diurnal cycle
#'
#'
#'@param lat Latitude in degrees
#'@param lon Longitude in degrees
#'@param dates A vector of dates to generate the diurnal cycles, will be extrapolated to hour, min, etc based on input to \code{by}
#'@param irr_mean The mean daily irradiance to match in the diurnal cycle (arbitrary units), must be the same length as \code{dates}
#'@param by Timestep to generate sequence of times, see \link{seq.POSIXt} for options
#'
#'@importFrom insol JD sunvector sunpos insolation normalvector
#'@importFrom plyr ddply
#'
#'
#'@export
create_irr_day_cycle = function(lat, lon, dates=as.POSIXct('1990-01-01'), irr_mean=1, by='hour'){
	
	#drop the timezone, insolation gives us everything in reference to UTC
	dates = as.POSIXct(format(dates, '%Y-%m-%d'), tz='UTC')
	
	datetime = seq(min(dates), max(dates) + 86400, by=by) #add a day to the max
	
	metjd=JD(datetime)
	
	sunv = sunvector(metjd, lat, lon, -6)
	
	zenith = sunpos(sunv)[,2]
	
	Idirdif = insolation(zenith, metjd, height=300, visibility=90, RH=70,tempK=20+273.15,O3=0.02,alphag=0.1)
	
	cos_inc_sfc=sunv%*%as.vector(normalvector(0,0)) ## or sum(sunv*normalvector(0,0))
	
	cos_inc_sfc[cos_inc_sfc<0]=0
	Isim  = Idirdif[,1] * cos_inc_sfc
	
	
	#Now, this is some awesome black magic to apply a day-specific conversion 
	# factor to each day. 
	out = data.frame(datetime, irr=Isim)
	out$day = as.POSIXct(trunc(out$datetime, units='days'))
	
	mean_correct = data.frame(day=dates, irr_mean)
	to_convert = merge(out, mean_correct, by='day')
	
	out = ddply(to_convert, 'day', function(df){ 
		conversion = df$irr_mean[1]/mean(df$irr)
		corrected  = df$irr*conversion 
		return(data.frame(datetime=df$datetime, irr=corrected))
		})
	
	#DROP that day col
	out$day = NULL
	
	return(out[order(out$datetime),])
}

