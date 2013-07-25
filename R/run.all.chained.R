
source('chained.GLM.lib.R')
library(stringr)

## Run all GLM models
origin = getwd()

driver.dir = 'D:/WilmaInputFiles2/Driver files'
model.dirs = Sys.glob('D:/WilmaRuns/JamoCanopyChained/WBIC_*')
model.ids = basename(model.dirs)
WBICs = str_extract(model.ids,'\\d+')  # WBICS as strings

empir.ice = read.table('../supporting files/empirical.ice.tsv', sep='\t', header=TRUE, as.is=TRUE)
wtemp.obs = read.table('../supporting files/wtemp.obs.tsv', sep='\t', as.is = TRUE, header=TRUE)

for(i in 1:length(model.ids)){
  
  driver.file = paste(driver.dir, '/', model.ids[i], '.csv', sep='')
  
  file.copy(driver.file, model.dirs[i])
  
  ## Write the lake-specific ice on/off data
  lake.ice.info = empir.ice[empir.ice$WBIC == as.numeric(WBICs[i]), ]
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
  run.chained.GLM(model.dirs[i], glm.path = 'D:/WILMA/GLM/1.2.2/glm.exe')
  
  ## Now use calibration data to output matched modeled data for validation
  output.cal.chained(model.dirs[i])
  
  
  ## Delete the output *.nc files if you want
  unlink(Sys.glob(file.path(model.dirs[i], '*.nc')))
  
  #Print info on where we are
  print(paste(i,model.ids[i]))
  
}


