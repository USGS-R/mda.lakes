library(rGLM)

driver.dir	<-	'D:/WiLMA/Driver files'		# where the driver files are
model.dirs	<-	Sys.glob('../GLM/Run/WBIC_*')	# where the .nml files are
glm.path	<-	"C:/Users/jread/Desktop/GLM_v1.2.0/bin/glm.exe"	# where glm.exe is
model.ids	<-	basename(model.dirs)
WBICs	<-	str_extract(model.ids,'\\d+')  # WBICS as strings

sensitivity.GLM	<-	function(WBICs,param,range,n=10){
	# WBICs:	a character array with folder IDs for each simulation
	# param:	parameter to be evaluated
	# range:	range of parameter to evaluated
	# n:		number of values of param to apply over range
	origin	<-	getwd()
	
	for (i in 1:length(WBICs)){
		setwd(run.dir[i])	# set to this lake's run directory
		source.nml	<-	read.nml('glm.nml','./')
		file.copy('glm.nml', 'glm.nml.orig')
	}
	
	
}