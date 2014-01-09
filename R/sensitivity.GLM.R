library(rGLM)
library(stringr)
source("GLM.functions.R")

model.dirs	<-	Sys.glob('../GLM/Run/WBIC_*')	# where the .nml files are

sensitivity.GLM	<-	function(model.dirs,param,range,year,n=10,driver.dir='D:/WiLMA/Driver files'){
	# model.dirs:	a character array with folder IDs for each simulation
	# param:	parameter to be evaluated
	# range:	range of parameter to evaluated
	# n:		number of values of param to apply over range
	
	glm.path	<-	"C:/Users/jread/Desktop/GLM_v1.2.0/bin/glm.exe"	# where glm.exe is
	model.ids <- basename(model.dirs)
	WBICs	<-	str_extract(model.ids,'\\d+')  # WBICS as strings
	
  ice.on <- getIceOn(WBICs,year)
	ice.off <- getIceOff(WBICs,year)
	num.lakes	<-	length(WBICs)
	response.matrix	<-	matrix(nrow=num.lakes,ncol=n)
	new.params	<-	seq(from=range[1],to=range[2],length.out=n)	# params for range, n=n
	origin	<-	getwd()
	
	for (j in 1:num.lakes){
    cat(model.ids[j])
		driver.file	<-	paste(driver.dir, '/', model.ids[j], '.csv', sep='')
		file.copy(driver.file, model.dirs[j])
		setwd(model.dirs[j])	# set to this lake's run directory
		# IF there is already a glm.nml, that means the last one might have failed...replace?
		file.copy('glm.nml', 'glm.nml.orig')
		source.nml	<-	read.nml('glm.nml','./')
		source.nml  <-  set.nml(source.nml,'start', ice.off[j])	# set to empir ice on for year [HANDLE NAs?]
		source.nml  <-  set.nml(source.nml,'stop', ice.on[j])  # set to empir ice on for year

		for (i in 1:n){
			source.nml	<-	set.nml(source.nml,param,new.params[i])	# set to new param value
			
			write.nml(source.nml, 'glm.nml', './')
			out = system2(glm.path, wait=TRUE, stdout=TRUE,stderr=TRUE)	# runs and writes .nc to directory
      hypo.temps<- get.sim.temps(run.dir[j])
			response.matrix[j,i]	<-	 mean(hypo.temps[,2],na.rm=TRUE)
      cat('done with j=');cat(j);cat('of ');cat(num.lakes);cat(' and i=');cat(i);cat('\n')
		}
		file.rename('glm.nml.orig', 'glm.nml')
    setwd(origin)
		
	}	
	response.matrix = data.frame(response.matrix)
  names(response.matrix) <- paste(param,'_',new.params,sep='')
  response.matrix <- cbind("WBICs"=WBICs,response.matrix)
  return(response.matrix)
}

get.sim.temps	<-	function(run.dir,remove=FALSE){
	# open .nc file, extract response variable value
	# ....
  lyrDz <- 0.25
  GLMnc  <-  getGLMnc(file='output.nc',folder='./')
  temps	<-	getTempGLMnc(GLMnc,lyrDz,ref='bottom',z.out=0) # DO JAS MEAN?
  nc_close(GLMnc)
	if (remove){
		# delete nc file ...
	}
	return(temps)
}

response.matrix <- sensitivity.GLM(model.dirs,param='Kw',range=c(0.2,5),year=1996,n=10)

write.table(response.matrix,file=paste('sensitivity_',param,'.tsv',sep=''),quote=FALSE,sep='\t',row.names=FALSE)