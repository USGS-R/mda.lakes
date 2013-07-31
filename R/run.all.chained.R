setwd("~/WiLMA/R")

library(ncdf4)
library(rGLM)

source('chained.GLM.lib.R')
source('GLM.functions.R')
library(stringr)

summaryTxt = '../GLM/Run/summary.txt'

## Run all GLM models
origin = getwd()

driver.dir = 'D:/WiLMA/Driver files'
model.dirs = Sys.glob('../GLM/Run/WBIC_*')
glm.path = "C:/Users/jread/Desktop/GLM_v1.2.0/bin/glm.exe"
model.ids = basename(model.dirs)
WBICs = str_extract(model.ids,'\\d+')  # WBICS as strings



empir.ice = read.table('../supporting files/empirical.ice.tsv', sep='\t', header=TRUE, as.is=TRUE)
wtemp.obs = read.table('../supporting files/wtemp.obs.tsv', sep='\t', as.is = TRUE, header=TRUE)

cat('WBIC\tStndErr\tnumPoints\n', file=summaryTxt)

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
  
  Wstr = getWstr(WBICs[i],method='JEKL')
  nml.args = list('coef_wind_drag'=0.0016*Wstr^0.33)
  print(nml.args)
  run.chained.GLM(model.dirs[i], glm.path = glm.path,nml.args, verbose=FALSE)
  
  ## Now use calibration data to output matched modeled data for validation
  output.cal.chained(model.dirs[i])
  
  
  ## Delete the output *.nc files if you want
  unlink(Sys.glob(file.path(model.dirs[i], '*.nc')))
  
  dat = read.table(file.path(model.dirs[i], 'cal.out.tsv'),header=TRUE)
  
  
  
  resids = dat$WTEMP-dat$WTEMP_MOD
  resids = resids[!is.na(resids)]
  if (length(resids)>3){
    stdErr = sqrt(sum(resids^2)/length(resids))
    lm = lm(dat$WTEMP~dat$WTEMP_MOD-1)
    print(paste(c('linear model standard error:',summary(lm)$sigma),collapse=' '))
    plot(dat$WTEMP,dat$WTEMP_MOD)
    lines(c(0,30),c(0,30))
  } else {stdErr = NA}
 
  print(paste(c('obs vs model standard error:',stdErr,', from',length(resids),'points'),collapse=' '))
  #Print info on where we are
  print(paste(i,model.ids[i]))
  
  
  print(getArea(WBICs[i]))

  cat(paste(c(WBICs[i],'\t',stdErr,'\t',length(resids),'\n'),collapse=''), file=summaryTxt,append=TRUE)
  dat = read.table(summaryTxt,header=TRUE)
  print(paste('mean SE:',mean(dat$StndErr,na.rm=TRUE),', median SE:',median(dat$StndErr,na.rm=TRUE),sep=''))

}


