#' @title Calculate core NECSC thermal metrics
#' 
#' 
#' 
#' 
#' @import glmtools
#' @import lubridate
#' @export
necsc_thermal_metrics_core = function(run.path, site_id){
	
	nc.file = file.path(run.path, 'output.nc')
	nml.file = file.path(run.path, 'glm2.nml')
	
	all_wtr      = get_temp(nc.file, reference='surface')
	bottom_wtr   = get_temp(nc.file, reference='bottom', z_out=1) #1m from bottom
	surface_wtr  = get_temp(nc.file, reference='surface', z_out=0) #surface temps
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
	
	#names(hypso) = c('depths', 'areas')
	hypso = interp_hypso(hypso, dz=0.1, force_zero_area = TRUE)
	
	
	## We pull ice on/off out first to split up later metric calcs
	ice_onoff = get_ice_onoff(all_ice, all_wtr)
	ice_onoff = fix_ice_on_off(ice_onoff, all_wtr, hypso)
	ice_onoff$ice_duration_days = NA
	ice_onoff$ice_duration_days[2:nrow(ice_onoff)] = as.numeric(ice_onoff$off[2:nrow(ice_onoff)] - ice_onoff$on[1:(nrow(ice_onoff)-1)], units='days')
	ice_onoff$ice_on_date = format(ice_onoff$on, '%Y-%m-%d')
	ice_onoff$ice_off_date = format(ice_onoff$off, '%Y-%m-%d')
	
	years = ice_onoff$year
	
	#output list
	misc.out = list()
	
	for(i in 1:length(years)){
		
		onoff = subset(ice_onoff, year==years[i])
		
		#need this for the surface and bottom calcs
		surf_year = subset(surface_wtr, year(DateTime) == years[i])
		bot_year  = subset(bottom_wtr, year(DateTime) == years[i])
			
		wtr = subset(all_wtr, DateTime >= onoff$off & DateTime <= onoff$on)
		bot_wtr = subset(bottom_wtr, DateTime >= onoff$off & DateTime <= onoff$on)
		ice = subset(all_ice, DateTime >= onoff$off & DateTime <= onoff$on)

		raw_ind = all_raw_date >= onoff$off & all_raw_date <= onoff$on
		raw.wtr = all_raw_wtr[,raw_ind]
		raw.vol = all_raw_vol[,raw_ind]
		run.time = all_raw_date[raw_ind]
		vols = all_raw_totv[raw_ind]
		raw_z = all_raw_z[,raw_ind]
		
		surfT = wtr[,(which.min(get.offsets(wtr))+1)]
		
		
		misc.out[['peak_temp']] = c(misc.out[['peak_temp']], getTempMax(wtr))
		
		misc.out[['coef_var_0-30']] = c(misc.out[['coef_var_0-30']], sd(surfT[1:30])/mean(surfT[1:30]))
		misc.out[['coef_var_30-60']] = c(misc.out[['coef_var_30-60']], sd(surfT[31:60])/mean(surfT[31:60]))
		
		
		##get stratification start_end, used here and later
		strat_periods = calc_stratified_periods(surf_year, bot_year, temp_thresh = 1, force_positive = TRUE)
		
		if(nrow(strat_periods) != 0){
			max_period = strat_periods[which.max(strat_periods$lengths), ]
			
			misc.out[['stratification_duration']] = c(misc.out[['stratification_duration']], max_period$lengths)
			misc.out[['stratification_onset_yday']] = c(misc.out[['stratification_onset_yday']], yday(max_period$onset))
			misc.out[['stratified_period_count']] = c(misc.out[['stratified_period_count']], nrow(strat_periods))
			misc.out[['stratified_avg_length']] = c(misc.out[['stratified_avg_length']], mean(strat_periods$lengths, na.rm=TRUE))
			
			#get bottom temp at onset of stratification
			
			misc.out[['bottom_temp_at_strat']] = c(misc.out[['bottom_temp_at_strat']], 
																							subset(bot_year, DateTime == max_period$onset)[,2])
			
		}else{
			misc.out[['stratification_duration']] = c(misc.out[['stratification_duration']], NA)
			misc.out[['stratification_onset_yday']] = c(misc.out[['stratification_onset_yday']], NA)
			misc.out[['stratified_period_count']] = c(misc.out[['stratified_period_count']], NA)
			misc.out[['stratified_avg_length']] = c(misc.out[['stratified_avg_length']], NA)
			misc.out[['bottom_temp_at_strat']] = c(misc.out[['bottom_temp_at_strat']], NA)
		}
		
		#Seasonal thermo depth mean
		la.wtr = wtr
		names(la.wtr) = tolower(names(wtr))
		
		t.d = ts.thermo.depth(la.wtr, na.rm=TRUE, seasonal=TRUE)
		
		start.end = getUnmixedStartEnd(wtr, ice, 1, arr.ind=TRUE)
		
		misc.out[['sthermo_depth_mean']] = c(misc.out[['sthermo_depth_mean']], mean(t.d$thermo.depth[start.end[1]:start.end[2]], na.rm=TRUE))
		
		
		# get schmidt stability annual sum
		s.s = ts.schmidt.stability(wtr, bathy = hypso, na.rm = TRUE)
		misc.out[['schmidt_daily_annual_sum']] = c(misc.out[['schmidt_daily_annual_sum']], sum(s.s$schmidt.stability))
		
		
		#gdd base 0, 5, 10
		dd0 = surf_year$temp_0 - 0
		dd5 = surf_year$temp_0 - 5
		dd10 = surf_year$temp_0 - 10
		
		misc.out[['gdd_wtr_0c']] = c(misc.out[['gdd_wtr_0c']], sum(dd0[dd0 > 0], na.rm=TRUE))
		misc.out[['gdd_wtr_5c']] = c(misc.out[['gdd_wtr_5c']], sum(dd5[dd5 > 0], na.rm=TRUE))
		misc.out[['gdd_wtr_10c']] = c(misc.out[['gdd_wtr_10c']], sum(dd10[dd10 > 0], na.rm=TRUE))
		
		#get JAS bottom and suf avg/max
		surf_year$month = month(surf_year$DateTime)
		bot_year$month = month(bot_year$DateTime)
		
		misc.out[['mean_surf_jas']] = c(misc.out[['mean_surf_jas']], mean(subset(surf_year, month %in% 7:9)[,2]))
		misc.out[['max_surf_jas']] = c(misc.out[['max_surf_jas']], max(subset(surf_year, month %in% 7:9)[,2]))
		
		misc.out[['mean_bot_jas']] = c(misc.out[['mean_bot_jas']], mean(subset(bot_year, month %in% 7:9)[,2]))
		misc.out[['max_bot_jas']] = c(misc.out[['max_bot_jas']], max(subset(bot_year, month %in% 7:9)[,2]))
		
		
		#get all months bottom and surf avg/max
		for(m in 1:12){
			mean_surf_name = tolower(paste0('mean_surf_', as.character(month(m, label=TRUE))))
			max_surf_name = tolower(paste0('max_surf_', as.character(month(m, label=TRUE))))
			mean_bot_name  = tolower(paste0('mean_bot_', as.character(month(m, label=TRUE))))
			max_bot_name  = tolower(paste0('max_bot_', as.character(month(m, label=TRUE))))
			
			misc.out[[mean_surf_name]] = c(misc.out[[mean_surf_name]], mean(subset(surf_year, month %in% m)[,2]))
			misc.out[[max_surf_name]] = c(misc.out[[max_surf_name]], max(subset(surf_year, month %in% m)[,2]))
			
			misc.out[[mean_bot_name]] = c(misc.out[[mean_bot_name]], mean(subset(bot_year, month %in% m)[,2]))
			misc.out[[max_bot_name]] = c(misc.out[[max_bot_name]], max(subset(bot_year, month %in% m)[,2]))
		}
	}
	
	#Build output data frame
	fOutput = data.frame(year=years)
	fOutput$site_id = site_id
	
	misc.names = names(misc.out)
	for(i in 1:length(misc.names)){
		fOutput[[misc.names[i]]] = misc.out[[misc.names[i]]]
	}
	
	fOutput = merge(fOutput, ice_onoff[, c('year', 'ice_duration_days', 'ice_on_date', 'ice_off_date')], by='year', all.x=TRUE)
	
	return(fOutput)
	
}
