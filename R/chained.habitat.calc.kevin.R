
#'@title Chained habitat out functoin for Krose metrics
#'@description
#'Calculates all the different metrics requested by Krose
#'
#'@import stringr
#'@import glmtools
#'@import rLakeAnalyzer
#'
#'@export
chained.habitat.calc.kevin = function(run.path, output.path=NULL, lakeid){
	
	nc.files = Sys.glob(file.path(run.path, '*.nc'))
	years = str_extract(basename(nc.files),"[0-9]+")
	
	if(missing(lakeid)){
		lakeid = basename(run.path)
	}
	
	misc.out = list()
	
	bad = rep(FALSE, length(years))
	
	#empir.ice = read.table(file.path(run.path, 'icecal.in.tsv'), sep='\t', header=TRUE)
	#empir.ice$DATE = as.POSIXct(empir.ice$DATE)
	
	#We need Hypsometry for some of Kevin's damn numbers
	nml.data = read_nml(file.path(run.path, 'glm2.nml'))
	bathy = getBathy(lakeid)
	
	bthy.areas = bathy$area #nml.data$morphometry$A*1000  #GLM file has areas in 1000's of m^2
	bthy.depths = bathy$depth #abs(nml.data$morphometry$H - max(nml.data$morphometry$H))
	
	bathy = data.frame(depths=bthy.depths, areas=bthy.areas)
	bathy = bathy[order(bathy$depths),]  #sort by depth, I think we want ascending
	
	for(i in 1:length(nc.files)){
		
		#Open the NC file and get data frame of wtr temp
		#GLMnc = nc_open(nc.files[i], readunlim=FALSE)
		
		GLMnc = nc.files[i]
		#test.get = ncvar_get(GLMnc,'temp')
		
		wtr = get_temp(GLMnc)
		ice = get_ice(GLMnc)
		surfT = get_temp(GLMnc, reference='surface', z_out=0)
		floorT = get_temp(GLMnc, reference='bottom', z_out=0)
		#floorT = floorT[4:nrow(floorT),]
		
		wtr1m = get_temp(GLMnc, reference='surface', z_out=1)
		#wtr1m = wtr1m[4:nrow(wtr1m),]
		
		#tmp = nc_open(GLMnc, readunlim=FALSE)
		raw.wtr = get_raw(GLMnc, 'temp')
		#nc_close(tmp)
		run.time = wtr[,1]
		
		
		#Drop the first 3 days
		#wtr = wtr[4:nrow(wtr), , drop=FALSE]
		#ice = ice[4:nrow(wtr),]
		#surfT = surfT[4:nrow(wtr)]
		#raw.wtr = raw.wtr#[,4:ncol(raw.wtr), drop=FALSE] #this is a matrix with a different orientation
		#run.time = run.time#[4:length(run.time)]
		
		censor.days = 0  #used for later functions to censor burn-in days
		
		#Make sure temps are in a sane range
		if(any(raw.wtr > 50, na.rm=TRUE) | any(raw.wtr < -20, na.rm=TRUE)){
			
			cat('Unreasonable temp values found:', lakeid, '\n')
			#nc_close(GLMnc)
			bad[i] = TRUE
			next
		}
		
		#Ok, sometimes the first modeled day gives a really bad temp value
		# at the surface. Drop those!
		if(any(raw.wtr[,1] > 30, na.rm=TRUE)){
			cat('Unreasonable first day temps found:', lakeid, '\n')
			#nc_close(GLMnc)
			bad[i] = TRUE
			next
		}
		
		
		#Make sure the last date is within 2 days of one of the ice-ons
		# If it isn't, then the model probably failed early
		if( !any(abs(difftime(run.time[length(run.time)],as.POSIXct(getIceOn(lakeid, as.numeric(years[i]) )), units='days')) < 2.1) ){
			cat('Wrong ice-off date:', lakeid, '\n')
			#nc_close(GLMnc)
			bad[i] = TRUE
			next
		}
		
		misc.out[['durStrat']] = c(misc.out[['durStrat']], getStratifiedDuration(wtr, ice, minStrat=0.5))
		
		jun1 = as.POSIXct(paste(years[i], '-06-01', sep=''))
		jul1 = as.POSIXct(paste(years[i], '-07-01', sep=''))
		jul31 = as.POSIXct(paste(years[i], '-07-31', sep=''))
		sep30 = as.POSIXct(paste(years[i], '-09-30', sep=''))
		
		la.wtr = get_temp(GLMnc, ref='surface')
		#la.wtr = la.wtr[4:nrow(la.wtr),] # drop those pesky intro days
		names(la.wtr) = tolower(names(la.wtr)) ## fix the header to make it 'rLA' compatible
		
		s.s = ts.schmidt.stability(la.wtr, bathy, na.rm=TRUE)
		
		misc.out[['max_schmidt_stability']] = c(misc.out[['max_schmidt_stability']], max(s.s[,2], na.rm=TRUE))
		
		misc.out[['mean_schmidt_stability_JAS']] = c(misc.out[['mean_schmidt_stability_JAS']], 
																								 mean(s.s[s.s$datetime >= jul1 & s.s$datetime <=sep30,2], na.rm=TRUE))
		
		misc.out[['mean_schmidt_stability_July']] = c(misc.out[['mean_schmidt_stability_July']], 
																									mean(s.s[s.s$datetime >= jul1 & s.s$datetime <=jul31,2], na.rm=TRUE))
		
		
		tmp = ts.thermo.depth(la.wtr, seasonal=TRUE, na.rm=TRUE)
		start.end = getUnmixedStartEnd(wtr, ice, 0.5, arr.ind=TRUE)
		
		misc.out[['SthermoD_mean']] = c(misc.out[['SthermoD_mean']], mean(tmp$thermo.depth[start.end[1]:start.end[2]], na.rm=TRUE))
		
		
		depths = get.offsets(la.wtr)
		
		whole.lake.temps = ts.layer.temperature(la.wtr, 0, max(depths), bathy, na.rm=TRUE)
		
		misc.out[['lake_average_temp']] = c(misc.out[['lake_average_temp']], mean(whole.lake.temps[,2], na.rm=TRUE))
		
		tmp = ts.meta.depths(la.wtr, seasonal=TRUE, na.rm=TRUE)
		
		epi.temps = ts.layer.temperature(la.wtr, 0, tmp$top, bathy, na.rm=TRUE)
		hypo.temps = ts.layer.temperature(la.wtr, tmp$bottom, max(depths), bathy, na.rm=TRUE)
		
		misc.out[['mean_epi_temp']] = c(misc.out[['mean_epi_temp']], mean(epi.temps$layer.temp[start.end[1]:start.end[2]], na.rm=TRUE))
		misc.out[['mean_hypo_temp']] = c(misc.out[['mean_hypo_temp']], mean(hypo.temps$layer.temp[start.end[1]:start.end[2]], na.rm=TRUE))
		
		misc.out[['mean_surf_temp']] = c(misc.out[['mean_surf_temp']], mean(surfT[,2]))
		misc.out[['mean_bottom_temp']] = c(misc.out[['mean_bottom_temp']], mean(floorT[,2]))
		
		misc.out[['mean_bottom_temp_365']] = c(misc.out[['mean_bottom_temp_365']],
																					 mean(c(floorT[,2], rep(4, 365 - length(floorT[,2])))))
		
		misc.out[['mean_surf_temp_365']] = c(misc.out[['mean_surf_temp_365']],
																				 mean(c(surfT[,2], rep(4, 365 - length(surfT[,2])))))
		
		misc.out[['mean_1m_temp']] = c(misc.out[['mean_1m_temp']], mean(wtr1m[,2], na.rm=TRUE))
		
		jun1 = as.POSIXct(paste(years[i], '-06-01', sep=''))
		jul1 = as.POSIXct(paste(years[i], '-07-01', sep=''))
		jul31 = as.POSIXct(paste(years[i], '-07-31', sep=''))
		sep30 = as.POSIXct(paste(years[i], '-09-30', sep=''))
		aug31 = as.POSIXct(paste(years[i], '-08-31', sep=''))
		
		misc.out[['mean_surf_JA']] = c(misc.out[['mean_surf_JA']],
																	 mean(getSurfaceT(wtr[wtr$DateTime >= jul1 & wtr$DateTime <= aug31, ])))
		
		## GDD calcs
		dd10 = surfT[,2] - 10
		dd5  = surfT[,2] - 5
		misc.out[['GDD_wtr_5c']] = c(misc.out[['GDD_wtr_5c']], sum(dd5[dd5 > 0], na.rm=TRUE))
		misc.out[['GDD_wtr_10c']] = c(misc.out[['GDD_wtr_10c']], sum(dd10[dd10 > 0], na.rm=TRUE))
		
		vols = get_raw(GLMnc,'Tot_V')
		vols = vols[c(-1,-2,-3)]
		
		#Units are in ML, so 1ML = 1000 m^3
		misc.out[['volume_mean_m_3']] = c(misc.out[['volume_mean_m_3']], mean(vols, na.rm=TRUE)*1000)
		misc.out[['simulation_length_days']] = c(misc.out[['simulation_length_days']], length(vols))
		
		mean.vol.temp = sum(get_raw(GLMnc,'temp')*get_raw(GLMnc,'V'), na.rm=TRUE)/sum(get_raw(GLMnc,'V'), na.rm=TRUE)
		misc.out[['mean_volumetric_temp']] = c(misc.out[['mean_volumetric_temp']], mean.vol.temp)
		
		
		#Cleanup this memory hog
		#nc_close(GLMnc)
		cat("Vols calculated", years[i], '\n')
	}
	
	#Build output data frame
	fOutput = data.frame(year=years[!bad])
	fOutput$lakeid = lakeid
	
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
