

#'@export
continuous.habitat.calc = function(run.path, output.path=NULL, lakeid){
	
	if(!(require(stringr) & require(glmtools))){
		stop('Need stringr and glmtools for chained.habitat.calc')
	}
	
	nc.file = file.path(run.path, 'output.nc')
	nml.file = file.path(run.path, 'glm2.nml')
	
	all_wtr      = get_temp(nc.file, reference='surface')
  all_ice      = get_ice(nc.file)
	all_raw_wtr  = get_raw(nc.file, 'temp')
	all_raw_vol  = get_raw(nc.file, 'V')
	all_raw_totv = get_raw(nc.file, 'Tot_V')
	all_raw_date = all_wtr[,1]
	all_raw_z    = get_raw(nc.file, 'z')
  
  ## bunch of stuff for opt-therm hab
	nml = read_nml(nml.file)
	hypso = get_hypsography(nml)
	kd = get_nml_value(nml, 'Kw')
	lat = get_nml_value(nml, 'latitude')
	lon = get_nml_value(nml, 'longitude')
	
  names(hypso) = c('depth', 'area')
	hypso = interp_hypso(hypso, dz=0.1, force_zero_area = TRUE)
	
	#also extract temp at the same resolution as hypsography. This will help us standardize across light and temp
	all_opt_wtr = get_temp(nc.file, z_out=hypso$depth, reference='surface')
	
	all_io = get_var(nc.file, 'I_0')
  
  
  ice_onoff = get_ice_onoff(all_ice, all_wtr)
	years = ice_onoff$year
	
	if(missing(lakeid)){
		lakeid = basename(run.path)
	}
	
	vol.tmp.ranges = c(26.6, 32,
										 26,   28,
										 27,   30, 
										 27,   32,
										 18,   31.1,
										 28,   29,
										 30,   31,
										 10.6, 11.2,
										 18.2, 28.2,
										 18,   22, 
										 19,   23, 
										 20.6, 23.2,
										 22,   23,
										 29,   100)
	
	height.tmp.ranges = vol.tmp.ranges
	
	day.tmp.ranges = vol.tmp.ranges
	
	#Convert to matricies
	vol.tmp.ranges = matrix(vol.tmp.ranges, ncol=2, byrow=TRUE)
	
	height.tmp.ranges = matrix(height.tmp.ranges, ncol=2, byrow=TRUE)
	
	day.tmp.ranges = matrix(day.tmp.ranges, ncol=2, byrow=TRUE)
	
	
	volumes.out = list()
	heights.out = list()
	days.out = list()
	
	misc.out = list()
	
	bad = rep(FALSE, length(years))
	
	#used for calcs which need the previous year's data available
	previous.wtr = NA 
	
	for(i in 1:length(years)){
		
		onoff = subset(ice_onoff, year==years[i])
		
		wtr = subset(all_wtr, DateTime >= onoff$off & DateTime <= onoff$on)
		ice = subset(all_ice, DateTime >= onoff$off & DateTime <= onoff$on)
		opt_wtr = subset(all_opt_wtr, DateTime >= onoff$off & DateTime <= onoff$on)
    io  = subset(all_io, DateTime >= onoff$off & DateTime <= onoff$on)
    
		surfT = wtr[,(which.min(get.offsets(wtr))+1)]
		
		raw_ind = all_raw_date >= onoff$off & all_raw_date <= onoff$on
		raw.wtr = all_raw_wtr[,raw_ind]
		raw.vol = all_raw_z[,raw_ind]
		run.time = all_raw_date[raw_ind]
		vols = all_raw_totv[raw_ind]
		raw_z = all_raw_z[,raw_ind]
		
		censor.days = 0
		
		#Iterate through all ranges and store in name-indexed list
		for(j in 1:nrow(vol.tmp.ranges)){
			#Name of volume temp range
			vol.name = paste('vol', vol.tmp.ranges[j,1], vol.tmp.ranges[j,2], sep='_')
			
			#Calc 
			tmp = volInTemp.GLM.continuous(raw.vol, raw.wtr, vol.tmp.ranges[j,1], vol.tmp.ranges[j,2], censor.days=censor.days)
			
			#add to vector
			volumes.out[[vol.name]] = c(volumes.out[[vol.name]], sum(tmp)*1000)
		}
		
		#Now do vertical length of water column in temperature range.
		for(j in 1:nrow(height.tmp.ranges)){
			height.name = paste('height', height.tmp.ranges[j,1], height.tmp.ranges[j,2], sep='_')
			tmp = heightInRange.GLM.continuous(raw.wtr, raw_z, height.tmp.ranges[j,1], height.tmp.ranges[j,2], censor.days=censor.days)
			
			heights.out[[height.name]] = c(heights.out[[height.name]], mean(tmp, na.rm=TRUE))
		}
		
		for(j in 1:nrow(day.tmp.ranges)){
			name = paste('days', day.tmp.ranges[j,1], day.tmp.ranges[j,2], sep='_')
			days.out[[name]] = c(days.out[[name]], getDaysBetweenT.continuous(getTemp(wtr), day.tmp.ranges[j,1], day.tmp.ranges[j,2]))
		}
		
		
		misc.out[['peak_temp']] = c(misc.out[['peak_temp']], getTempMax(wtr))
		
		misc.out[['durStrat']] = c(misc.out[['durStrat']], getStratifiedDuration(wtr, ice, minStrat=0.5))
		
		
		misc.out[['dateOver5']] = c(misc.out[['dateOver5']], getFirstDayAboveT(wtr, 5))
		misc.out[['dateOver6']] = c(misc.out[['dateOver6']], getFirstDayAboveT(wtr, 6))
		misc.out[['dateOver8.9']] = c(misc.out[['dateOver8.9']], getFirstDayAboveT(wtr, 8.9))
		misc.out[['dateOver21']] = c(misc.out[['dateOver21']], getFirstDayAboveT(wtr, 21))
		misc.out[['dateOver20']] = c(misc.out[['dateOver20']], getFirstDayAboveT(wtr, 20))
		misc.out[['dateOver18']] = c(misc.out[['dateOver18']], getFirstDayAboveT(wtr, 18))
		
		
		misc.out[['coef_var_0_30']] = c(misc.out[['coef_var_0_30']], sd(surfT[1:30])/mean(surfT[1:30]))
		misc.out[['coef_var_30_60']] = c(misc.out[['coef_var_30_60']], sd(surfT[31:60])/mean(surfT[31:60]))
		
		tmpDay = 1:30
		misc.out[['post_ice_warm_rate']] = c(misc.out[['post_ice_warm_rate']], lm(surfT[1:30]~tmpDay)$coefficients[2])
		
		jun1 = as.POSIXct(paste(years[i], '-06-01', sep=''))
		jul1 = as.POSIXct(paste(years[i], '-07-01', sep=''))
		jul31 = as.POSIXct(paste(years[i], '-07-31', sep=''))
		sep30 = as.POSIXct(paste(years[i], '-09-30', sep=''))
		
		surf = data.frame(DateTime=run.time, wtr_0=surfT)
		
		misc.out[['mean_surf_jul']] = c(misc.out[['mean_surf_jul']],
																		mean(surf[surf$DateTime >= jul1 & surf$DateTime <= jul31, 2]))
		
		misc.out[['mean_surf_JAS']] = c(misc.out[['mean_surf_JAS']],
																		mean(surf[surf$DateTime >= jul1 & surf$DateTime <= sep30, 2]))
		
		
		misc.out[['spring_days_in_10.5_15.5']] = c(misc.out[['spring_days_in_10.5_15.5']],
																							 getDaysBetweenT(wtr[wtr$DateTime < jun1, ], 10.5, 15.5))
		
		
		##Add in the thermo-optical indices
		#
		#Optical and thermal habitat thresholds from Lester et al 2004
		# temp  = 11 to 25 C
		# light = 8  to 68 Lux
		# Converted light thresholds to W/m^2 using Luminous efficacy of daylight from Littlefair 1985 (105 lm/W)
		# light = 0.0762 to 0.6476

 		oti = opti_thermal_habitat(opt_wtr, io, kd, lat, lon, hypso, irr_thresh = c(0.0762, 0.6476), 
 															 wtr_thresh=c(11,25), interp_hour=TRUE, area_type="benthic")
 		
		misc.out[['optic_hab_8_64']] = c(misc.out[['optic_hab_8_64']], oti$opti_hab)
		misc.out[['thermal_hab_11_25']] = c(misc.out[['thermal_hab_11_25']], oti$therm_hab)
		misc.out[['optic_thermal_hab']] = c(misc.out[['optic_thermal_hab']], oti$opti_therm_hab)
 		
 		
		oti_surf = opti_thermal_habitat(opt_wtr, io, kd, lat, lon, hypso, irr_thresh = c(0.0762, 0.6095), 
																		wtr_thresh=c(11,25), interp_hour=TRUE, area_type="surface")
 		
		misc.out[['optic_hab_8_64_surf']] = c(misc.out[['optic_hab_8_64_surf']], oti_surf$opti_hab)
		misc.out[['thermal_hab_11_25_surf']] = c(misc.out[['thermal_hab_11_25_surf']], oti_surf$therm_hab)
		misc.out[['optic_thermal_hab_surf']] = c(misc.out[['optic_thermal_hab_surf']], oti_surf$opti_therm_hab)
		
		hypso = getBathy(lakeid)
		misc.out[['lake_benthic_area']] = c(misc.out[['lake_benthic_area']], sum(benthic_areas(hypso$depth, hypso$area)))
		misc.out[['lake_surface_area']] = c(misc.out[['lake_surface_area']], getArea(lakeid))
		
		#Use rLA, headers must be lowercase
		la.wtr = wtr
		names(la.wtr) = tolower(names(wtr))
		
		t.d = ts.thermo.depth(la.wtr, na.rm=TRUE)
		m.d = ts.meta.depths(la.wtr, na.rm=TRUE)
		
		start.end = getUnmixedStartEnd(wtr, ice, 0.5, arr.ind=TRUE)
		
		misc.out[['SthermoD_mean']] = c(misc.out[['SthermoD_mean']], mean(t.d$thermo.depth[start.end[1]:start.end[2]], na.rm=TRUE))
		misc.out[['SmetaTopD_mean']] = c(misc.out[['SmetaTopD_mean']], mean(m.d$top[start.end[1]:start.end[2]], na.rm=TRUE))
		misc.out[['SmetaBotD_mean']] = c(misc.out[['SmetaBotD_mean']], mean(m.d$bottom[start.end[1]:start.end[2]], na.rm=TRUE))
		
		## Get epi and hypo volumes
		water.level = water.level.glm.continuous(raw_z)
		
		meta.top.heights = water.level - m.d$top
		meta.bot.heights = water.level - m.d$bottom
		
		epi.vols = volsAboveHeight.GLM.continuous(raw_z, raw.vol, meta.top.heights)
		hyp.vols = volsBelowHeight.GLM.continuous(raw_z, raw.vol, meta.bot.heights)
		
		if(diff(start.end) < 1){
			mean.epi.vol = NA
			mean.hyp.vol = NA
		}else{
			mean.epi.vol = mean(epi.vols[start.end[1]:start.end[2]], na.rm=TRUE)
			mean.hyp.vol = mean(hyp.vols[start.end[1]:start.end[2]], na.rm=TRUE)
		}
		
		misc.out[['mean_epi_vol']] = c(misc.out[['mean_epi_vol']], mean.epi.vol)
		misc.out[['mean_hyp_vol']] = c(misc.out[['mean_hyp_vol']], mean.hyp.vol)
		misc.out[['mean_epi_hypo_ratio']] = c(misc.out[['mean_epi_hypo_ratio']], mean.epi.vol/mean.hyp.vol)
		
		
		## GDD calcs
		dd10 = surfT - 10
		dd5  = surfT - 5
		misc.out[['GDD_wtr_5c']] = c(misc.out[['GDD_wtr_5c']], sum(dd5[dd5 > 0], na.rm=TRUE))
		misc.out[['GDD_wtr_10c']] = c(misc.out[['GDD_wtr_10c']], sum(dd10[dd10 > 0], na.rm=TRUE))
		
		
		#Units are in ML, so 1ML = 1000 m^3
		misc.out[['volume_mean_m_3']] = c(misc.out[['volume_mean_m_3']], mean(vols, na.rm=TRUE)*1000)
		misc.out[['volume_sum_m_3_day']] = c(misc.out[['volume_sum_m_3']], sum(vols, na.rm=TRUE)*1000)
		misc.out[['simulation_length_days']] = c(misc.out[['simulation_length_days']], length(vols))
		
		if(!is.na(previous.wtr)){
			
			dur0to4 = difftime(wtr$DateTime[1], previous.wtr$DateTime[nrow(previous.wtr)], units='days')
			dur0to4 = trunc(dur0to4)
			
			#previous year
			lastOct30 = as.POSIXct(paste((as.numeric(years[i])-1), '-10-30', sep=''))
			#this year stop
			thisJul1 = as.POSIXct(paste(years[i], '-07-01', sep=''))
			
			dur0to4 = dur0to4 + getDaysBetweenT.continuous(previous.wtr[previous.wtr$DateTime > lastOct30, ], 0, 4)
			dur0to4 = dur0to4 + getDaysBetweenT.continuous(wtr[wtr$DateTime < thisJul1, ], 0, 4)
			
			misc.out[['winter_dur_0-4']] = c(misc.out[['winter_dur_0-4']], dur0to4)
			
		}else{
			misc.out[['winter_dur_0-4']] = c(misc.out[['winter_dur_0-4']], NA)
		}
		
		previous.wtr = wtr
		
		cat("Vols calculated", years[i], '\n')
	}
	
	#Build output data frame
	fOutput = data.frame(year=years[!bad])
	fOutput$lakeid = lakeid
	
	vol.names = names(volumes.out)
	for(i in 1:length(vol.names)){
		fOutput[[vol.names[i]]] = volumes.out[[vol.names[i]]]
	}
	
	height.names = names(heights.out)
	for(i in 1:length(height.names)){
		fOutput[[height.names[i]]] = heights.out[[height.names[i]]]
	}
	days.names = names(days.out)
	for(i in 1:length(days.names)){
		fOutput[[days.names[i]]] = days.out[[days.names[i]]]
	}
	misc.names = names(misc.out)
	for(i in 1:length(misc.names)){
		fOutput[[misc.names[i]]] = misc.out[[misc.names[i]]]
	}
	
	#Output!!
	if(!is.null(output.path)){
		write.table(fOutput, output.path, row.names=FALSE, sep='\t')
	}else{
		return(fOutput)
	}
}



