
library(ncdf4)
library(rGLM)

source('Libraries/chained.GLM.lib.R')
source('Libraries/GLM.functions.R')
library(stringr)



## Run all GLM models
origin = getwd()

driver.dir = 'D:/WilmaDrivers/07-30'
model.dirs = Sys.glob('D:/WilmaRuns/2014-04-30/WBIC_*')
glm.path = "D:/WILMA/GLM/1.2/glm.exe"
model.ids = basename(model.dirs)
WBICs = str_extract(model.ids,'\\d+')  # WBICS as strings

wndRef = 0.00145
wndMethod = 'Markfort'


empir.ice = read.table('../supporting files/empirical.ice.tsv', sep='\t', header=TRUE, as.is=TRUE)
wtemp.obs = read.table('../supporting files/wtemp.obs.tsv', sep='\t', as.is = TRUE, header=TRUE)

with.cal.i = WBICs%in%unique(wtemp.obs$WBIC)
WBICs = WBICs[with.cal.i]
model.dirs = model.dirs[with.cal.i]
model.ids = model.ids[with.cal.i]

for(i in 1:length(model.ids)){
  
  driver.file = paste(driver.dir, '/', model.ids[i], '.csv', sep='')

  file.copy(driver.file, model.dirs[i])

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
  Kw   = getClarity(WBICs[i], default.if.null=TRUE)
  
  nml.args = list('Kw' = Kw)# reverting...
  
  run.chained.GLM(model.dirs[i], glm.path = glm.path, nml.args, verbose=FALSE, only.cal=TRUE)
  
  ## Now use calibration data to output matched modeled data for validation
  output.cal.chained(model.dirs[i])
  
  
  ## Delete the output *.nc files if you want
  unlink(Sys.glob(file.path(model.dirs[i], '*.nc')))
  cat(WBICs[i], '\n')
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

write.table(all.cal,'../Output/all.cal.tsv', sep='\t', row.names=FALSE)


