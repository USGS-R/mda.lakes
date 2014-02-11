## Run a chained GLM run

iscondor = FALSE
if(iscondor){
	install.packages("ncdf4_1.4.zip", lib='./rLibs', repos=NULL)
}


run.chained.GLM = function(run.dir, glm.path,nml.args=NULL, verbose=TRUE, only.cal=FALSE){
  	# run.dir is the home for all model inputs
  	# glm.path is the path to the glm.exe (including the exe)
	# only.cal is boolean to only run cal/val years
	
	# I don't like doing this in a function, but you must 
	# to properly run GLM
	origin = getwd()
	setwd(run.dir)

	## Key Paths and parameters


	## Open template NML file
	source.nml = read.nml('glm.nml','./')
	# stash original
	file.copy('glm.nml', 'glm.nml.orig')

	# Ice on/off Dates
	#Get the ice on/off dates
	ice.in = read.table('icecal.in.tsv', as.is=TRUE, header=TRUE, sep='\t')

	ice.in$DATE = as.POSIXct(ice.in$DATE)
	ice.in = ice.in[order(ice.in$DATE), ]


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
	nml.start = as.POSIXct(get.nml(source.nml,'start'))
	nml.end = as.POSIXct(get.nml(source.nml,'stop'))


	# Intersect the two (Max of starts, min of ends)
	# If we have ice-off before nml.start or ice-on after nml.end, truncate
	rmI = s.starts <= nml.start | s.ends >= nml.end
	s.starts = s.starts[!rmI]
	s.ends = s.ends[!rmI]


	#Iterate runs, appending output name with year.

	for(i in 1:length(s.starts)){
		
		#Edit and output NML
		source.nml <- set.nml(source.nml,argList=list('start'=strftime(s.starts[i], format="%Y-%m-%d %H:%M:%S"),
			'stop'=strftime(s.ends[i], format="%Y-%m-%d %H:%M:%S"),
		  'out_fn'=paste('output', strftime(s.starts[i],'%Y'), sep='')))
		
    
    if (!is.null(nml.args)){source.nml <- set.nml(source.nml,argList=nml.args)}
		write.nml(source.nml, 'glm.nml', './')
		
		#Runs this iteration of the model.
    if (!verbose){stdout=FALSE; stderr=FALSE} else {stdout=""; stderr=""}
		out = system2(glm.path, wait=TRUE, stdout=stdout,stderr=stderr)

		
	}
	#bring the original back
	file.rename('glm.nml.orig', 'glm.nml')

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

	lake.cal.data = read.table('cal.in.tsv', sep='\t', header=TRUE)

	nc.files = Sys.glob('output*.nc')
	glm.ncs = list()

	for(i in 1:length(nc.files)){
		glm.ncs[[i]] = nc_open(nc.files[i])
	}

	## Subsample run to get water temp data at 4D obs points
  
	
	lake.cal.dates = unique(lake.cal.data$DATETIME)
	lake.cal.depths = sort(unique(lake.cal.data$DEPTH))
  
  # create single wtr data.frame for all years (all nc files)
	wtr = getTempGLMnc(glm.ncs[[1]],ref='surface',z.out=lake.cal.depths)
	for(i in 2:length(glm.ncs)){
	  wtr = rbind(wtr, getTempGLMnc(glm.ncs[[i]],ref='surface',z.out=lake.cal.depths))
	}


  # add additional row for modeled temp which is NaN
	lake.cal.data$WTEMP_MOD = NaN
  
  # trim down data.frame for model to just contain the sample dates
	tmp = wtr[as.Date(wtr[,1],origin="CDT") %in% as.Date(lake.cal.dates),]
  
  # lookup matches for depth and time
	depthLookup = match(lake.cal.data$DEPTH, lake.cal.depths)
	datesLookup = match(as.Date(lake.cal.data$DATETIME),as.Date(tmp[,1]))


	for(j in 1:nrow(lake.cal.data)){
	  lake.cal.data$WTEMP_MOD[j] = tmp[datesLookup[j], (depthLookup[j]+1)]
	}

  #Close these pesky memory hogs
	for(i in 1:length(glm.ncs)){
	  nc_close(glm.ncs[[i]])
	}
  
	#out.fname = paste(runs.dir, '/WBIC_', cal.wbics[i], '/cal.csv', sep='')
	write.table(lake.cal.data, 'cal.out.tsv', row.names=FALSE, sep='\t')

	
	
	setwd(origin)

}

