library(rGLM)
library(stringr)
source("GLM.functions.R")

model.dirs	<-	Sys.glob('D:/WiLMA/GLM/Run/WBIC_*')	# where the .nml files are
stop.mmdd <- '09-30'

sensitivity.GLM	<-	function(model.dirs,param,param.seq,year,mode='relative'){
	# model.dirs:	a character array with folder IDs for each simulation
	# param:	parameter to be evaluated
	# param.seq:	parameter values (if mode==absolute) to evaluated

	if (!any(mode=='absolute' | mode=='relative')){stop(paste('mode ',mode,' not supported',sep=''))}
	
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

      		param.list	<-	get.params(param.name=param,param.seq=param.seq,WBIC=WBICs[j],mode=mode)
			argName	<-	param.list$argName
			new.params	<-	param.list$argVals
			
			file.copy(paste(driver.dir,driver.file,sep=''), model.dirs[j])
			setwd(model.dirs[j])	# set to this lake's run directory
			
			# IF there is already a glm.nml, that means the last one might have failed...replace?
			file.copy('glm.nml', 'glm.nml.orig')
			source.nml	<-	read.nml('glm.nml','./')
			
			stop.date <- paste(substr(ice.off[j],1,4),'-',stop.mmdd,sep='')
			source.nml  <-  set.nml(source.nml,'start', ice.off[j])	# set to empir ice on for year [HANDLE NAs?]
			source.nml  <-  set.nml(source.nml,'stop', stop.date)  # set to empir ice on for year
      
      		for (i in 1:num.params){
				source.nml  <-	set.nml(source.nml,argName=argName,new.params[i])	# set to new param value

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
  	names(response.matrix) <- paste(param,'_',mode,'_',param.seq,sep='')
  	response.matrix <- cbind("WBICs"=WBICs,response.matrix)
  	return(response.matrix)
}

get.params	<-	function(param.name,param.seq,WBIC,mode='relative'){
	
	argName	<-	param.name

	if (mode=='absolute'){
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
			initial.val	<-	getCanopy(WBIC)
			print(initial.val)
			for (i in 1:length(param.seq)){
				Wstr	<-	getWstr(WBIC=WBIC,canopy=initial.val*param.seq[i])
				print(Wstr)
				argVals[i]	<-	getCD(Wstr=Wstr)
			}
		} else if (param.name=='Kw'){
			argVals	<-	param.seq*getClarity(WBIC)
		}
	}
	return(list(argName=argName,argVals=argVals))
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

sens.bump	<-	0.01
sens.param	<-	'Kw'
sens.mode	<-	'relative'
calc.sens	<-	c(0.5,1,1.5,2)
param.seq	<-	sort(c(calc.sens-sens.bump,calc.sens+sens.bump))
sens.mode	<-	'relative'
response.matrix <- sensitivity.GLM(model.dirs[1:10],param=sens.param,param.seq=param.seq,mode=sens.mode,year=1996)

write.table(response.matrix,file=paste('sensitivity_',sens.mode,'_',sens.param,'.tsv',sep=''),quote=FALSE,sep='\t',row.names=FALSE)