library(rGLM)

model.dirs	<-	Sys.glob('../GLM/Run/WBIC_*')	# where the .nml files are

sensitivity.GLM	<-	function(model.dirs,param,range,n=10,driver.dir='D:/WiLMA/Driver files'){
	# model.dirs:	a character array with folder IDs for each simulation
	# param:	parameter to be evaluated
	# range:	range of parameter to evaluated
	# n:		number of values of param to apply over range
	
	glm.path	<-	"C:/Users/jread/Desktop/GLM_v1.2.0/bin/glm.exe"	# where glm.exe is
	WBICs	<-	str_extract(model.ids,'\\d+')  # WBICS as strings
	
	num.lakes	<-	length(WBICs)
	response.matrix	<-	matrix(nrow=num.lakes,ncol=n)
	new.params	<-	seq(from=range[1],to=range[2],length.out=n)	# params for range, n=n
	origin	<-	getwd()
	
	for (j in 1:num.lakes){
		driver.file	<-	paste(driver.dir, '/', model.ids[j], '.csv', sep='')
		file.copy(driver.file, model.dirs[j])
		setwd(run.dir[j])	# set to this lake's run directory
		source.nml	<-	read.nml('glm.nml','./')
		# IF there is already a glm.nml, that means the last one might have failed...replace?
		file.copy('glm.nml', 'glm.nml.orig')
		for (i in 1:n){
			source.nml	<-	set.nml(source.nml,param,new.params[i])	# set to new param value
			write.nml(source.nml, 'glm.nml', './')
			out = system2(glm.path, wait=TRUE, stdout=TRUE,stderr=TRUE)	# runs and writes .nc to directory
			response.matrix[j,i]	<-	get.response.val(run.dir[j])
		}
	}	
}

get.response.val	<-	function(run.dir,remove=TRUE){
	# open .nc file, extract response variable value
	# ....
	# nc_close

	if (remove){
		# delete nc file ...
	}
	return(value)
}