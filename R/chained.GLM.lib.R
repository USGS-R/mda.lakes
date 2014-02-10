## Run a chained GLM run

iscondor = FALSE
if(iscondor){
	install.packages("ncdf4_1.4.zip", lib='./rLibs', repos=NULL)
}


run.chained.GLM = function(run.dir, glm.path,nml.args=NULL, verbose=TRUE){
  #run.dir is the home for all model inputs
  #glm.path is the path to the glm.exe (including the exe)
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


	#Iterate runs, appending output name with year.

	for(i in 1:length(s.starts)){
		
		#Edit and output NML
	  source.nml <- set.nml(source.nml,'start',strftime(s.starts[i], format="%Y-%m-%d %H:%M:%S"))
	  source.nml <- set.nml(source.nml,'stop',strftime(s.ends[i], format="%Y-%m-%d %H:%M:%S"))
	  source.nml <- set.nml(source.nml,'out_fn',paste('output', strftime(s.starts[i],'%Y'), sep=''))
		if (!is.null(nml.args)){
      for (a in 1:length(nml.args)){
        source.nml <- set.nml(source.nml,names(nml.args[a]),nml.args[[a]])
      }
	  }
		write.nml(source.nml, 'glm.nml', './')
		
		#Rusn this iteration of the model.
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

	cal.d = read.table('cal.in.tsv', sep='\t', header=TRUE)

	nc.files = Sys.glob('output*.nc')
	glm.ncs = list()

	for(i in 1:length(nc.files)){
		glm.ncs[[i]] = nc_open(nc.files[i])
	}

	## Subsample run to get water temp data at 4D obs points
  
	
	lake.cal.data = cal.d
	lake.cal.dates = unique(lake.cal.data$DATETIME)
	lake.cal.depths = sort(unique(lake.cal.data$DEPTH))
	wtr = getTempGLMnc(glm.ncs[[1]],ref='surface',z.out=lake.cal.depths)
	for(i in 2:length(glm.ncs)){
	  wtr = rbind(wtr, getTempGLMnc(glm.ncs[[i]],ref='surface',z.out=lake.cal.depths))
	}
	

	lake.mod.data = data.frame(WBIC=lake.cal.data$WBIC, 
							   DATETIME=lake.cal.data$DATETIME, 
							   DEPTH=lake.cal.data$DEPTH, 
							   WTEMP=lake.cal.data$DEPTH*NaN)

	lake.cal.data$WTEMP_MOD = NaN

	tmp = wtr[as.Date(wtr$[,1])==lake.cal.dates,]

	depthLookup = match(lake.cal.data$DEPTH, lake.cal.depths)
	datesLookup = match(lake.cal.data$DATETIME, lake.cal.dates)


	for(j in 1:nrow(lake.cal.data)){
	  lake.cal.data$WTEMP_MOD[j] = tmp[datesLookup[j], depthLookup[j]]
	}

  #Close these pesky memory hogs
	for(i in 1:length(glm.ncs)){
	  nc_close(glm.ncs[[i]])
	}
  
	#out.fname = paste(runs.dir, '/WBIC_', cal.wbics[i], '/cal.csv', sep='')
	write.table(lake.cal.data, 'cal.out.tsv', row.names=FALSE, sep='\t')

	
	
	setwd(origin)

}

