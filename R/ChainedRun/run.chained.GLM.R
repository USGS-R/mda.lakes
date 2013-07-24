## Run a chained GLM run

# Import Libraries
source('GLM.nml.R')


## Key Paths and parameters

#Must be the complete path to your GLM binary
#Best not to use relative path here
glm.path = 'D:/WILMA/GLM/1.2.2/glm.exe'


## Open template NML file
source.nml = read.nml('glm.nml','./')
# stash original
file.copy('glm.nml', 'glm.nml.orig')

# Ice on/off Dates
#Get the ice on/off dates
ice.in = read.csv('icecal.in.csv', as.is=TRUE)

ice.in$DATE = as.POSIXct(ice.in$DATE)
ice.in = ice.in[order(ice.in[, 1]), ]


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
	source.nml$time$start = strftime(s.starts[i], format="%Y-%m-%d %H:%M:%S")
	source.nml$time$stop = strftime(s.ends[i], format="%Y-%m-%d %H:%M:%S")
	
	source.nml$output$out_fn = paste('output', strftime(s.starts[i],'%Y'), sep='')
	
	write.nml(source.nml, 'glm.nml', './')
	
	#Run this iteration of the model.
	out = system2(glm.path, wait=TRUE)
	
	print(out)
	
}

#bring the original back
file.rename('glm.nml.orig', 'glm.nml')




