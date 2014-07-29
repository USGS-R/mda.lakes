## Run a chained GLM run

iscondor = FALSE
if(iscondor){
	install.packages("ncdf4_1.4.zip", lib='./rLibs', repos=NULL)
}

Sys.setenv(TZ='GMT')

run.chained.GLM = function(run.dir, glm.path, nml.args=NULL, verbose=TRUE, only.cal=FALSE){
  	# run.dir is the home for all model inputs
  	# glm.path is the path to the glm.exe (including the exe)
	# only.cal is boolean to only run cal/val years
	# I don't like doing this in a function, but you must 
	# to properly run GLM
	origin = getwd()
	setwd(run.dir)

	if(!file.exists(glm.path)){
		stop('glm.path does not point correctly to a GLM executable')
	}
	
	## Key Paths and parameters
	

	## Open template NML file
	source.nml = read_nml('glm.nml')
	# stash original
	file.copy('glm.nml', 'glm.nml.orig')

	# Ice on/off Dates
	#Get the ice on/off dates
	ice.in = read.table('icecal.in.tsv', as.is=TRUE, header=TRUE, sep='\t')

	ice.in$DATE = as.POSIXct(ice.in$DATE)
	ice.in = ice.in[order(ice.in$DATE), ]

  #temporary hack
  if(ice.in$DATE[nrow(ice.in)] > as.POSIXct('2011-12-31')){
    ice.in$DATE[nrow(ice.in)] = as.POSIXct('2011-12-31')
  }
  

	# Find complete seasons (pairs of off/on dates)
	s.starts = NA
	s.ends = NA
	class(s.starts) = c("POSIXct", "POSIXt")
	class(s.ends) = c("POSIXct", "POSIXt")
	season.i = 1

	for(i in 1:nrow(ice.in)){
		abs(as.double(ice.in$DATE[i] - s.starts[season.i], units="days")) < 365
		#browser()
		if(ice.in$ON.OFF[i] == 'off'){# Must start at an "off" 
			s.starts[season.i] = ice.in$DATE[i]
				
		}else if(!is.na(s.starts[season.i]) && 
			abs(as.double(ice.in$DATE[i] - s.starts[season.i], units="days")) < 365){
			s.ends[season.i] = ice.in$DATE[i]
			
			season.i = season.i + 1
		}
	}
	
	# all the seasonal start dates
	s.starts = s.starts[1:length(s.ends)]
	if (only.cal){
		# only sim years where val dates fall within s.starts and s.ends
		sim.idx	<-	vector(length=length(s.starts))
		if(!file.exists('cal.in.tsv')){
			stop('No Cal Data for this lake')
		}
		
		cal.d = read.table('cal.in.tsv', sep='\t', header=TRUE)
		lake.cal.dates = unique(cal.d$DATETIME)
		
		for (j in 1:length(s.starts)){
			if (any(as.Date(lake.cal.dates)>=as.Date(s.starts[j]) & as.Date(lake.cal.dates)<=as.Date(s.ends[j]))){
				sim.idx[j] = TRUE
			}
		}
		# trim off sim years that don't have validation samples
		s.starts	<-	s.starts[sim.idx]
		s.ends	<-	s.ends[sim.idx]
	}
	# Figure out the years to model with start/end dates
	# Find start/stop dates in existing NML
	nml.start = as.POSIXct(get_nml_value(source.nml,'start'))
	nml.end = as.POSIXct(get_nml_value(source.nml,'stop'))


	# Intersect the two (Max of starts, min of ends)
	# If we have ice-off before nml.start or ice-on after nml.end, truncate
	rmI = s.starts <= nml.start | s.ends >= nml.end
	s.starts = s.starts[!rmI]
	s.ends = s.ends[!rmI]

    #Iterate runs, appending output name with year.
    for(i in seq_len(length(s.starts))){
      
      #Edit and output NML
      source.nml <- set_nml(source.nml,arg_list=list('start'=strftime(s.starts[i], format="%Y-%m-%d %H:%M:%S"),
                                                    'stop'=strftime(s.ends[i], format="%Y-%m-%d %H:%M:%S"),
                                                    'out_fn'=paste('output', strftime(s.starts[i],'%Y'), sep='')))
      
      
      if (!is.null(nml.args)){source.nml <- set_nml(source.nml,arg_list=nml.args)}
      write_nml(source.nml, file_out = 'glm.nml')
      
      #Runs this iteration of the model.
      if (!verbose){stdout=FALSE; stderr=FALSE} else {stdout=""; stderr=""}
      out = system2(glm.path, wait=TRUE, stdout=stdout,stderr=stderr)
      if(out != 0){
      	warning('GLM is sick')
      }
      
      
    }
  
	
	#bring the original back
	file.rename('glm.nml.orig', 'glm.nml')

	setwd(origin)
}


run.prefixed.chained.GLM = function(run.dir, glm.path, nml.args=NULL, verbose=TRUE){
  require(rGLM)
  #run.dir is the home for all model inputs
  #glm.path is the path to the glm.exe (including the exe)
  # I don't like doing this in a function, but you must 
  # to properly run GLM
  origin = getwd()
  setwd(run.dir)
  
  ## Key Paths and parameters
  
  ## Open template NML file
  source.nml = read_nml('glm.nml','./')
  # stash original
  file.copy('glm.nml', 'glm.nml.orig')
  
  # Ice on/off Dates
  #Get the ice on/off dates
  ice.in = read.table('icecal.in.tsv', as.is=TRUE, header=TRUE, sep='\t')
  
  ice.in$DATE = as.POSIXct(ice.in$DATE)
  ice.in = ice.in[order(ice.in$DATE), ]
  
  #temporary hack
  if(ice.in$DATE[nrow(ice.in)] > as.POSIXct('2011-12-31')){
    ice.in$DATE[nrow(ice.in)] = as.POSIXct('2011-12-31')
  }
  
  
  # Find complete seasons (pairs of off/on dates)
  s.starts = NA
  s.ends = NA
  class(s.starts) = c("POSIXct", "POSIXt")
  class(s.ends) = c("POSIXct", "POSIXt")
  season.i = 1
  
  for(i in 1:nrow(ice.in)){
    abs(as.double(ice.in$DATE[i] - s.starts[season.i], units="days")) < 365
    #browser()
    if(ice.in$ON.OFF[i] == 'off'){# Must start at an "off" 
      s.starts[season.i] = ice.in$DATE[i]
      
    }else if(!is.na(s.starts[season.i]) && 
               abs(as.double(ice.in$DATE[i] - s.starts[season.i], units="days")) < 365){
      s.ends[season.i] = ice.in$DATE[i]
      
      season.i = season.i + 1
    }
  }
  s.starts = s.starts[1:length(s.ends)]
  
  # Figure out the years to model with start/end dates
  # Find start/stop dates in existing NML
  nml.start = as.POSIXct(source.nml$time$start)
  nml.end = as.POSIXct(source.nml$time$stop)
  
  
  # Intersect the two (Max of starts, min of ends)
  # If we have ice-off before nml.start or ice-on after nml.end, truncate
  rmI = s.starts <= nml.start | s.ends >= nml.end
  s.starts = s.starts[!rmI]
  s.ends = s.ends[!rmI]
  
  three.day.file = "ShortWave,LongWave,AirTemp,RelHum,WindSpeed,Rain,Snow
218.1,276.7,-1.80,84.7,7.80,0.000,0.002
296.5,206.8,-3.12,79.1,3.89,0.000,0.019
222.3,245.9,-1.25,84.0,5.50,0.001,0.009"
  
  # Prep prefix met data
  prefix = read.csv(textConnection(three.day.file), header=TRUE)
  
  full.met = read.csv(get_nml_value(source.nml, 'meteo_fl'), header=TRUE)#, colClasses=c(time="POSIXct"))
  #stash original
  file.copy(get_nml_value(source.nml, 'meteo_fl'), 'met.csv.orig')
  
  
  #Let's change that prefix data!
  
  for(i in 1:length(s.starts)){
    startI = which(strftime(s.starts[i], format="%Y-%m-%d %H:%M:%S") == full.met$time)
    full.met[(startI-3):(startI-1),-1] = prefix
    
    
  }
  #Write the met data with the new prefixes before iceoffs
  write.csv(full.met, get_nml_value(source.nml, 'meteo_fl'), row.names=FALSE, quote=FALSE)
  
  
  #Iterate runs, appending output name with year.
  for(i in 1:length(s.starts)){  
    
    #Edit and output NML, start this thing 3 days early
    source.nml <- set_nml(source.nml,'start',strftime(s.starts[i]-as.difftime(3, unit="days"), format="%Y-%m-%d %H:%M:%S"))
    source.nml <- set_nml(source.nml,'stop',strftime(s.ends[i], format="%Y-%m-%d %H:%M:%S"))
    source.nml <- set_nml(source.nml,'out_fn',paste('output', strftime(s.starts[i],'%Y'), sep=''))
    if (!is.null(nml.args)){
      for (a in 1:length(nml.args)){
        source.nml <- set_nml(source.nml,names(nml.args[a]),nml.args[[a]])
      }
    }
    write_nml(source.nml, file_out = 'glm.nml')
    
    #Rusn this iteration of the model.
    if (!verbose){stdout=FALSE; stderr=FALSE} else {stdout=""; stderr=""}
    
    out = system2(glm.path, wait=TRUE, stdout=stdout,stderr=stderr)
    cat(out,'\n')
    
  }
  #bring the original back
  file.rename('glm.nml.orig', 'glm.nml')
  file.rename('met.csv.orig', get_nml_value(source.nml, 'meteo_fl'))
  
	setwd(origin)
}


output.cal.chained = function(run.dir){

	require(ncdf4)
	origin = getwd()
	setwd(run.dir)
	#output.cal.chained
	#This tries to take calibration data and join it with model output data
	# THis particular version deals with the damn chained runs

	if(!file.exists('cal.in.tsv')){
	  stop('No Cal Data for this lake')
	}

	nc.files = Sys.glob('output*.nc')	# create vector of nc files in this directory
	
  	if (length(nc.files>0)){
	
		lake.cal.data = read.table('cal.in.tsv', sep='\t', header=TRUE)
    
    	## Subsample run to get water temp data at 4D obs points

    	lake.cal.dates = unique(lake.cal.data$DATETIME)
    	lake.cal.depths = sort(unique(lake.cal.data$DEPTH))
    
    	# create single wtr data.frame for all years (all nc files)
		wtr.tmp = get_temp(file = nc.files[1], reference = 'surface', z_out = lake.cal.depths)
		# trim it down to matching dates
		wtr = wtr.tmp[as.Date(wtr.tmp[,1]) %in% as.Date(lake.cal.dates),]
    
    	if (length(nc.files)>1){
      		for(i in 2:length(nc.files)){
            # rbind is slow...
				    wtr.tmp = get_temp(file = nc.files[i], reference = 'surface', z_out = lake.cal.depths)
				    # trim it down to matching dates
				    wtr.tmp = wtr.tmp[as.Date(wtr.tmp[,1]) %in% as.Date(lake.cal.dates),]
        		wtr = rbind(wtr,wtr.tmp)
      		}
    	}
    
    	# add additional row for modeled temp which is NA
    	lake.cal.data$WTEMP_MOD = NA
    
    	# lookup matches for depth and time
    	depthLookup = match(lake.cal.data$DEPTH, lake.cal.depths)
    	datesLookup = match(as.Date(lake.cal.data$DATETIME),as.Date(wtr[,1]))
    
    
    	for(j in 1:nrow(lake.cal.data)){
      		lake.cal.data$WTEMP_MOD[j] = wtr[datesLookup[j], (depthLookup[j]+1)]
    	}
    
    	#out.fname = paste(runs.dir, '/WBIC_', cal.wbics[i], '/cal.csv', sep='')
    	write.table(lake.cal.data, 'cal.out.tsv', row.names=FALSE, sep='\t')
  	}
	
	setwd(origin)

}


get.wtr.chained.prefix = function(run.dir){
  
  require(ncdf4)
  require(glmtools)
  origin = getwd()
  setwd(run.dir)
  #output.cal.chained
  #This tries to take calibration data and join it with model output data
  # THis particular version deals with the damn chained runs
  
  
  nc.files = Sys.glob('output*.nc')
  
  
  for(i in 1:length(nc.files)){
    glm.ncs[[i]] = nc_open(nc.files[i])
  }
  
  ## Subsample run to get water temp data at 4D obs points
  
  wtr = get_temp(file = nc.files[i], 0.25)
  wtr = wtr[4:nrow(wtr), ] #Drop the first three burn-in days
  
  for(i in 2:length(glm.ncs)){
    tryCatch({
      tmp = getTempGLMnc(glm.ncs[[i]], lyr.elevations=getElevGLM(wtr))
      
      wtr = rbind(wtr, tmp[4:nrow(tmp), ]) #Drop the first three days which were "burn-in" days
      
    }, error=function(e){}) #If we error, just skip this year
  }

  #write.table(wtr, 'output.wtr', sep='\t', col.names=TRUE, row.names=FALSE)
  
  #Close these pesky memory hogs
  for(i in 1:length(glm.ncs)){
    nc_close(glm.ncs[[i]])
  }
  
  setwd(origin)
  
  return(wtr)
  
}

run.prefixed.chained.kd.scenario.GLM = function(run.dir, glm.path, nml.args=NULL, verbose=TRUE){
  require(glmtools)
  #run.dir is the home for all model inputs
  #glm.path is the path to the glm.exe (including the exe)
  # I don't like doing this in a function, but you must 
  # to properly run GLM
  origin = getwd()
  setwd(run.dir)
  
  ## Key Paths and parameters
  
  ## Open template NML file
  source.nml = read_nml('glm.nml')
  # stash original
  file.copy('glm.nml', 'glm.nml.orig')
  
  # Ice on/off Dates
  #Get the ice on/off dates
  ice.in = read.table('icecal.in.tsv', as.is=TRUE, header=TRUE, sep='\t')
  
  ice.in$DATE = as.POSIXct(ice.in$DATE)
  ice.in = ice.in[order(ice.in$DATE), ]
  
  #temporary hack
  if(ice.in$DATE[nrow(ice.in)] > as.POSIXct('2011-12-31')){
    ice.in$DATE[nrow(ice.in)] = as.POSIXct('2011-12-31')
  }
  
  kd.in = read.table('kd.scenario.tsv', as.is=TRUE, header=TRUE, sep='\t')
  
  
  # Find complete seasons (pairs of off/on dates)
  s.starts = NA
  s.ends = NA
  class(s.starts) = c("POSIXct", "POSIXt")
  class(s.ends) = c("POSIXct", "POSIXt")
  season.i = 1
  
  for(i in 1:nrow(ice.in)){
    abs(as.double(ice.in$DATE[i] - s.starts[season.i], units="days")) < 365
    #browser()
    if(ice.in$ON.OFF[i] == 'off'){# Must start at an "off" 
      s.starts[season.i] = ice.in$DATE[i]
      
    }else if(!is.na(s.starts[season.i]) && 
               abs(as.double(ice.in$DATE[i] - s.starts[season.i], units="days")) < 365){
      s.ends[season.i] = ice.in$DATE[i]
      
      season.i = season.i + 1
    }
  }
  s.starts = s.starts[1:length(s.ends)]
  
  # Figure out the years to model with start/end dates
  # Find start/stop dates in existing NML
  nml.start = as.POSIXct(source.nml$time$start)
  nml.end = as.POSIXct(source.nml$time$stop)
  
  
  # Intersect the two (Max of starts, min of ends)
  # If we have ice-off before nml.start or ice-on after nml.end, truncate
  rmI = s.starts <= nml.start | s.ends >= nml.end
  s.starts = s.starts[!rmI]
  s.ends = s.ends[!rmI]
  
  three.day.file = "ShortWave,LongWave,AirTemp,RelHum,WindSpeed,Rain,Snow
  218.1,276.7,-1.80,84.7,7.80,0.000,0.002
  296.5,206.8,-3.12,79.1,3.89,0.000,0.019
  222.3,245.9,-1.25,84.0,5.50,0.001,0.009"
  
  # Prep prefix met data
  prefix = read.csv(textConnection(three.day.file), header=TRUE)
  
  full.met = read.csv(get_nml_value(source.nml, 'meteo_fl'), header=TRUE)#, colClasses=c(time="POSIXct"))
  #stash original
  file.copy(get_nml_value(source.nml, 'meteo_fl'), 'met.csv.orig')
  
  
  #Let's change that prefix data!
  
  for(i in 1:length(s.starts)){
    startI = which(strftime(s.starts[i], format="%Y-%m-%d %H:%M:%S") == full.met$time)
    full.met[(startI-3):(startI-1),-1] = prefix
    
    
  }
  #Write the met data with the new prefixes before iceoffs
  write.csv(full.met, get_nml_value(source.nml, 'meteo_fl'), row.names=FALSE, quote=FALSE)
  
  
  #Iterate runs, appending output name with year.
  for(i in 1:length(s.starts)){  
    
    #Edit and output NML, start this thing 3 days early
    source.nml <- set_nml(source.nml,'start',strftime(s.starts[i]-as.difftime(3, unit="days"), format="%Y-%m-%d %H:%M:%S"))
    source.nml <- set_nml(source.nml,'stop',strftime(s.ends[i], format="%Y-%m-%d %H:%M:%S"))
    source.nml <- set_nml(source.nml,'out_fn',paste('output', strftime(s.starts[i],'%Y'), sep=''))
    if (!is.null(nml.args)){
      for (a in 1:length(nml.args)){
        source.nml <- set_nml(source.nml,names(nml.args[a]),nml.args[[a]])
      }
    }
    
    #Write new Kd value
    this.kd = kd.in$kd[kd.in$year == as.numeric(strftime(s.starts[i],'%Y'))]
    source.nml <- set_nml(source.nml, 'Kw', this.kd)
    
    write_nml(source.nml, file_out = 'glm.nml')
    
    #Runs this iteration of the model.
    if (!verbose){stdout=FALSE; stderr=FALSE} else {stdout=""; stderr=""}
    
    out = system2(glm.path, wait=TRUE, stdout=stdout,stderr=stderr)
    cat(out,'\n')
    
  }
  #bring the original back
  file.rename('glm.nml.orig', 'glm.nml')
  file.rename('met.csv.orig', get_nml_value(source.nml, 'meteo_fl'))
  
  setwd(origin)
}

