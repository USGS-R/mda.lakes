library(rGLM)
library(stringr)
source("GLM.functions.R")
wndRef = 0.00140
model.dirs	<-	Sys.glob('D:/WiLMA/GLM/Run/WBIC_*')	# where the .nml files are
stop.mmdd <- '09-30'

sensitivity.GLM	<-	function(model.dirs,param,param.seq,year,mode='relative'){
	# model.dirs:	a character array with folder IDs for each simulation
	# param:	parameter to be evaluated
	# param.seq:	parameter values (if mode==absolute) to evaluated

	
	if (mode!='absolute' | mode!='relative'){stope(paste('mode ',mode,' not supported',sep=''))}
	
	driver.dir	<-	'D:/WiLMA/Driver files/'
	glm.path	<-	"C:/Users/jread/Desktop/GLM_v1.2.0/bin/glm.exe"	# where glm.exe is
	model.ids	<-	basename(model.dirs)
	WBICs	<-	str_extract(model.ids,'\\d+')  # WBICS as strings
	
	ice.on <- getIceOn(WBICs,year)
	#ice.off <- getIceOff(WBICs,year) # not needed if stopping at stop.mmdd
	num.lakes	<-	length(WBICs)
	response.matrix	<-	matrix(nrow=num.lakes,ncol=n)
	new.params	<-	seq(from=range[1],to=range[2],length.out=n)	# params for range, n=n
	origin	<-	getwd()
	
	for (j in 1:num.lakes){
		setwd(origin)
		cat(model.ids[j]);cat(' ')
		driver.file	<-	paste(model.ids[j], '.csv', sep='')
		if (mod=="relative"){
			new.params	<-	seq(from=range[1],to=range[2],length.out=n)	# params for range, n=n
		}
		
		# ****driver can be missing, ice.off and on can be NA****
    	if (driver.file %in% dir(driver.dir) & !is.na(ice.on[j])){
			if (param=='hc'){
				Wstr<- vector(length=n)
        		for (i in 1:n){Wstr[i]<-getWstr(WBIC=WBICs[j],canopy=new.params[i])}
      		}
      
			file.copy(paste(driver.dir,driver.file,sep=''), model.dirs[j])
			setwd(model.dirs[j])	# set to this lake's run directory
			# IF there is already a glm.nml, that means the last one might have failed...replace?
			file.copy('glm.nml', 'glm.nml.orig')
			source.nml	<-	read.nml('glm.nml','./')
			
			stop.date <- paste(substr(ice.off[j],1,4),'-',stop.mmdd,sep='')
			source.nml  <-  set.nml(source.nml,'start', ice.off[j])	# set to empir ice on for year [HANDLE NAs?]
			source.nml  <-  set.nml(source.nml,'stop', stop.date)  # set to empir ice on for year
      
      		for (i in 1:n){
				if (param=='hc'){
					# lake-specific calculation
					param.value <- wndRef*Wstr[i]^0.33
					source.nml  <-	set.nml(source.nml,argName="coef_wind_drag",param.value)	# set to new param value
				} else {
          			source.nml  <-	set.nml(source.nml,argName=param,new.params[i])	# set to new param value
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
  	names(response.matrix) <- paste(param,'_',new.params,sep='')
  	response.matrix <- cbind("WBICs"=WBICs,response.matrix)
  	return(response.matrix)
}

get.params	<-	function(param.name,param.seq,WBIC,mode='relative'){
	if (mode='absolute'){
		argVals	<-	seq(from=range[1],to=range[2],length.out=n)
		if (param.name=='hc'){ # don't switch these up for others that don't need the function
			argName="coef_wind_drag"
			for (i in 1:n){
				argVals[i]	<-	getWstr(WBIC=WBIC,canopy=argVals[i])
			}
		}
	} else {
		
	}
	
		
		
		for (i in 1:n){
			if (mode=='absolute'){
				Wstr[i]	<-	getWstr(WBIC=WBIC,canopy=new.params[i])
			} else {
				initial.val	<-	getCanopy(WBIC)
				Wstr[i]	<-	getWstr(WBIC=WBIC,canopy=new.params[i])
			}
					
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

sens.param<-'Kw'
response.matrix <- sensitivity.GLM(model.dirs,param=sens.param,param.seq=seq(0.2,4,length.out=10),year=1996)

write.table(response.matrix,file=paste('sensitivity_',sens.param,'.tsv',sep=''),quote=FALSE,sep='\t',row.names=FALSE)