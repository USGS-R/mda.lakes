#condor.run.all.chained.newcal.oncondor

dir.create('rLibs')
contrib = 'http://www.bathybase.org/bin/windows/contrib/2.15'
install.packages('ncdf4', lib='./rLibs', contriburl=contrib)
install.packages('stringr', lib='./rLibs', contriburl=contrib)
install.packages('rGLM', lib='./rLibs', contriburl=contrib)

library(ncdf4, lib.loc='rLibs')
library(rGLM, lib.loc='rLibs')
library(stringr, lib.loc='rLibs')
source('chained.GLM.lib.R')
source('GLM.functions.R')

unzip('drivers.zip', exdir='drivers')
unzip('subdaily_fixed_nmls.zip', exdir='runs')

args = as.numeric(commandArgs(trailingOnly=TRUE))

sw = args[1:2] #intercept slope
lw = args[3:4]
rh = args[5:6]

## Run all GLM models
origin = getwd()

driver.dir = file.path(origin,'drivers')
model.dirs = Sys.glob(file.path(origin, 'runs/WBIC_*'))
glm.path = file.path(origin, "glm.exe")
model.ids = basename(model.dirs)
WBICs = str_extract(model.ids,'\\d+')  # WBICS as strings

empir.ice = read.table('empirical.ice.tsv', sep='\t', header=TRUE, as.is=TRUE)
wtemp.obs = read.table('wtemp.obs.tsv', sep='\t', as.is = TRUE, header=TRUE)
airt.mult = read.table('nldas.airt.coef.tsv', sep='\t', header=TRUE)

with.cal.i = WBICs%in%unique(wtemp.obs$WBIC)
WBICs = WBICs[with.cal.i]
model.dirs = model.dirs[with.cal.i]
model.ids = model.ids[with.cal.i]

for(i in 100:length(model.ids)){
	
	driver.file = paste(driver.dir, '/', model.ids[i], '.csv', sep='')
	
	if(!file.exists(driver.file)){
		next
	}
	
	# Change drivers according to multip-offsets
	driver.data = read.table(driver.file, sep=',', header=TRUE, as.is=TRUE)
	
	driver.data$ShortWave =  sw[1] + driver.data$ShortWave * sw[2]
	driver.data$LongWave =  lw[1] + driver.data$LongWave * lw[2]
	driver.data$RelHum =  rh[1] + driver.data$RelHum * rh[2]
	
	air = airt.mult[airt.mult$WBIC == as.numeric(WBICs[i]), ]
	if(nrow(air) < 1){
		air = c(0, 1) #default to no change
	}else{
		air = c(air$b[1], air$m[1])
	}
	
	driver.data$AirTemp =  air[1] + driver.data$AirTemp * air[2]
	
	write.table(driver.data, driver.file, row.names=FALSE, sep=',', quote=FALSE)
	
	#still need to move this into sim directory
	file.rename(driver.file, paste(model.dirs[i], '/', model.ids[i], '.csv', sep=''))
	
	## Write the lake-specific ice on/off data
	lake.ice.info = empir.ice[empir.ice$WBIC == as.numeric(WBICs[i]), ]
	lake.ice.info = lake.ice.info[lake.ice.info$DATE != '', ]
	if(nrow(lake.ice.info) == 0){
		next
	}
	write.table(lake.ice.info, file.path(model.dirs[i], 'icecal.in.tsv'), row.names=FALSE, sep='\t')
	
	## Write the lake-specific validation observation data
	lake.wtemp.obs = wtemp.obs[wtemp.obs$WBIC == as.numeric(WBICs[i]),]
	if(nrow(lake.wtemp.obs) == 0){
		next
	}
	write.table(lake.wtemp.obs, file.path(model.dirs[i], 'cal.in.tsv'), row.names=FALSE, sep='\t')
	
	## Should be ready, run chained model
	#glm.path must be absolute path, not relative
	
	#h_s	<-	getCanopy(WBICs[i],default.if.null=TRUE)
	#Wstr = getWstr(WBICs[i],method=wndMethod,canopy=h_s*h_s.mult[m])
	Kw   = getClarity(WBICs[i], default.if.null=TRUE, source.file='annual_mean_secchi.txt')
	
	nml.args = list('Kw' = Kw, 'ce'=0.0013, 'ch'=0.0013)
	
	run.chained.GLM(model.dirs[i], glm.path = glm.path, nml.args, verbose=FALSE, only.cal=TRUE)
	
	## Now use calibration data to output matched modeled data for validation
	output.cal.chained(model.dirs[i])
	
	
	## Delete the output *.nc files if you want
	unlink(Sys.glob(file.path(model.dirs[i], '*.nc')))
	cat(i/length(model.ids)*100, '%\t', WBICs[i], '\n')
}


all.cal = data.frame()

for(i in 1:length(model.ids)){
	cal.file = file.path(model.dirs[i], 'cal.out.tsv')
	if(!file.exists(cal.file)){
		next
	}
	
	tmp = read.table(cal.file, sep='\t', header=TRUE)
	all.cal = rbind(all.cal, tmp)
}

write.table(all.cal,'all.cal.tsv', sep='\t', row.names=FALSE)


