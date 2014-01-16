library(rGLM)
library(stringr)
source("GLM.functions.R")

model.dirs	<-	Sys.glob('D:/WiLMA/GLM/Run/WBIC_*')	# where the .nml files are
stop.mmdd <- '09-30'

sensitivity.GLM	<-	function(model.dirs,param.name,param.seq,year,sens.mode='relative'){
	# model.dirs:	a character array with folder IDs for each simulation
	# param:	parameter to be evaluated
	# param.seq:	parameter values (if mode==absolute) to evaluated

	
	driver.dir	<-	'D:/WiLMA/Driver files/'
	glm.path	<-	"C:/Users/jread/Desktop/GLM_v1.2.0/bin/glm.exe"	# where glm.exe is
	model.ids	<-	basename(model.dirs)
	WBICs	<-	str_extract(model.ids,'\\d+')  # WBICS as strings
	
	#ice.on <- getIceOn(WBICs,year)  # not needed if stopping at stop.mmdd
	ice.off <- getIceOff(WBICs,year)
	num.lakes	<-	length(WBICs)
	num.params	<-	length(param.seq)
	response.matrix	<-	matrix(nrow=num.lakes,ncol=num.params)
	origin	<-	getwd()
	
	for (j in 1:num.lakes){
		setwd(origin)
		cat(model.ids[j]);cat(' ')
		driver.file	<-	paste(model.ids[j], '.csv', sep='')
		
		# ****driver can be missing, ice.off and on can be NA****
    	if (driver.file %in% dir(driver.dir) & !is.na(ice.off[j])){
			
			if (param.name!='RT'){
				param.list	<-	get.params(param.name=param.name,param.seq=param.seq,WBIC=WBICs[j],sens.mode=sens.mode)
				argName	<-	param.list$argName
				new.params	<-	param.list$argVals
			} else if (param.name=='RT.air'){
				param.name = 'RT'
				temp.scheme="air"
			} else if (param.name=='RT.ave'){
				param.name = 'RT'
				temp.scheme="ave"
			}
			
		
			file.copy(paste(driver.dir,driver.file,sep=''), model.dirs[j])
			setwd(model.dirs[j])	# set to this lake's run directory
			# IF there is already a glm.nml, that means the last one might have failed...replace?
			file.copy('glm.nml', 'glm.nml.orig')
			source.nml	<-	read.nml('glm.nml','./')

			
			stop.date <- paste(substr(ice.off[j],1,4),'-',stop.mmdd,sep='')
			source.nml  <-  set.nml(source.nml,'start', ice.off[j])	# set to empir ice on for year [HANDLE NAs?]
			source.nml  <-  set.nml(source.nml,'stop', stop.date)  # set to empir ice on for year
      
      		for (i in 1:num.params){
				if (param.name=='RT'){
					source.nml	<-	set.up.RT.run(WBICs[j],source.nml,param.seq,temp.scheme="air") # some of this should happen outside this inner loop
					
				} else {
					source.nml  <-	set.nml(source.nml,argName=argName,new.params[i])	# set to new param value
				}
				write.nml(source.nml, 'glm.nml', './')

				sim.val <- tryCatch({
						out = system(glm.path, intern=FALSE, show.output.on.console =FALSE)  # runs and writes .nc to directory
						hypo.temps<- get.sim.temps(run.dir[j])
						season.hyp <- tail(hypo.temps[,2],91)
						mean(season.hyp,na.rm=TRUE)
					}, warning = function(war) {
						print(paste("MY_WARNING:  ",war))
						# warning handler picks up where error was generated
						hypo.temps<- get.sim.temps(run.dir[j])
						season.hyp <- tail(hypo.temps[,2],91)
						return(mean(season.hyp,na.rm=TRUE))
					}, error = function(err) {
						print(paste("MY_ERROR:  ",err))
						return(NA)
					}
				) # END tryCatch
				response.matrix[j,i]  <-   sim.val
				cat('done with j=');cat(j);cat(' of ');cat(num.lakes);cat(' and i=');cat(i);cat('\n')
			}
			# after all param sims, rename .orig to .nml
      		file.rename('glm.nml.orig', 'glm.nml')
		} else { # driver fails or file does not exist
			cat('skipping ');cat(model.ids[j]);cat('\n')
			response.matrix[j,] <- NA # will already be NA because it is built as such
    	}
	} # done with all lake simes
	setwd(origin)
	response.matrix = data.frame(response.matrix)
  	names(response.matrix) <- paste(param.name,'_',sens.mode,'_',param.seq,sep='')
  	response.matrix <- cbind("WBICs"=WBICs,response.matrix)
  	return(response.matrix)
}

get.params	<-	function(param.name,param.seq,WBIC,sens.mode='relative'){
	
	if (!any(sens.mode=='absolute' | sens.mode=='relative')){
		stop(paste('mode ',sens.mode,' not supported',sep=''))
	}
	
	if (param.name='RT'){stop('RT param not supported yet')}
	
	argName	<-	param.name

	if (sens.mode=='absolute'){
		argVals	<-	param.seq
		if (param.name=='hc'){ # don't switch these up for others that don't need the function
			argName="coef_wind_drag"
			for (i in 1:length(param.seq)){
				Wstr	<-	getWstr(WBIC=WBIC,canopy=argVals[i])
				argVals[i]	<-	getCD(Wstr=Wstr)
			}
		}
	} else {
		argVals	<-	param.seq*NA
		if (param.name=='hc'){ # canopy height
		  argName="coef_wind_drag"
			initial.val	<-	getCanopy(WBIC,default.if.null=TRUE)
			print(initial.val)
			for (i in 1:length(param.seq)){
				Wstr	<-	getWstr(WBIC=WBIC,canopy=initial.val*param.seq[i])
				argVals[i]	<-	getCD(Wstr=Wstr)
			}
		} else if (param.name=='Kw'){
			Kd	<-	getClarity(WBIC,default.if.null=TRUE) # default kd given if kd not found
			argVals	<-	param.seq*Kd
		}
	}
	return(list(argName=argName,argVals=argVals))
}

set.up.RT.run	<-	function(WBIC,source.nml,param.seq,temp.scheme="air"){
	# modify all related RT elements in nml:
	# INFLOW
	source.nml  <-	set.nml(source.nml,argName="inflow_factor",param.seq[i])
	source.nml  <-	set.nml(source.nml,argName="num_inflows",1)
	source.nml  <-	set.nml(source.nml,argName="num_inflows",c("inflow"))
	source.nml  <-	set.nml(source.nml,argName="inflow_fl",c("inflow.csv"))
	source.nml  <-	set.nml(source.nml,argName="inflow_varnum",2)
	source.nml  <-	set.nml(source.nml,argName="inflow_vars",c("FLOW","TEMP"))
	# OUTFLOW
	source.nml  <-	set.nml(source.nml,argName="outflow_factor",param.seq[i])
	source.nml  <-	set.nml(source.nml,argName="num_outlet",1)
	out.elv	<-	source.nml$morphometry$base_elev+source.nml$init_profiles$lake_depth
	source.nml  <-	set.nml(source.nml,argName="outl_elvs",out.elv)
	# bsn_len_outl? bsn_wid_outl?
	
	# get inflow and outflow files set in proper directory
	RT	<-	get.residence.time(WBIC,default.if.null=TRUE)	# will return ave RT if null
	flow	<-	get.flow.RT(source.nml,RT) 	# daily flow average to attain RT value
	time	<-	seq.Date(from=as.Date(source.nml$time$start),to=as.Date(source.nml$time$stop),by=1)
	
	temperature	<-	rep(5,length(time)) # USE get.air.temps.....
	flow	<-	rep(flow,length(time))
	write.flow(time,flow,temperature,dir='./',file.in=inflow.csv,file.out=outflow.csv)
	 #### 
	return(source.nml)
}

get.flow.RT	<-	function(nml,RT){
	# calculates flow (ML/day) required for a given residence time (RT; in days)
	
	#from nml, get hypso curve, initial height and calculate an estimate of volume
	m2.conv	<-	1000
	ML.conv	<-	1/1000
	area	<-	nml$morphometry$A*m2.conv	# area of each slice in m2
	height	<-	nml$morphometry$H-nml$morphometry$base_elev # in meters
	
	volume	<-	sum(area[-1]*diff(height))*ML.conv	# in ML (verified for Mendota)
	
	flow	<-	volume/RT
	return(flow)
}

get.air.temps	<-	function(time,dir,file.met){
	# opens GLM met driver file, extracts air temps for a given set of dates (time)
	# interpolates over NAs if found (???)
	return(air.temps)
}
write.flow	<-	function(time,flow,temperature,dir,file.in=inflow.csv,file.out=outflow.csv){
	# time is an array of daily times (e.g., "2011-01-03")
	# flow is an array of daily streamflow in ML/day (Convert from m3/s by multiplying by 86.4)
	# temperature is an array of daily stream temperatures (same length as time)
	
	# write.flow() writes inflow and outflow files for GLM input
	write.inflow	<-	data.frame("time"=time,"inflow"=flow,"temperature"=temperature)
	write.outflow	<-	data.frame("time"=time,"outflow"=flow)
	
	write.table(write.inflow,file=paste(dir,file.in,sep=''),quote=FALSE,sep=',',row.names=FALSE)
	write.table(write.outflow,file=paste(dir,file.out,sep=''),quote=FALSE,sep=',',row.names=FALSE)
}

get.sim.temps	<-	function(run.dir,remove=FALSE){
	# open .nc file, extract response variable value
	# ....
	lyrDz <- 0.25
	if ('output.nc' %in% dir('./') & file.info("./output.nc")$size/1000000 > 1){
		GLMnc  <-  getGLMnc(file='output.nc',folder='./')
		temps  <-	getTempGLMnc(GLMnc,lyrDz,ref='bottom',z.out=0) # DO JAS MEAN? # getting back elv>0?
		nc_close(GLMnc)
		if (remove){
      		# delete nc file ...
		}
  	} else {temps=data.frame('DateTime'=NA,'wtr_'=NA)}
  
	return(temps)
}


sens.param	<-	'RT.air'
sens.mode	<-	'relative'
param.seq	<-	seq(.3,1.7,by=0.1)
response.matrix <- sensitivity.GLM(model.dirs,param.name=sens.param,param.seq=param.seq,sens.mode=sens.mode,year=1996)

write.table(response.matrix,file=paste('sensitivity_',sens.mode,'_',sens.param,'.tsv',sep=''),quote=FALSE,sep='\t',row.names=FALSE)