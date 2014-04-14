## Run all condor
## This submit file creates condor submit files for workflow that
## does a few things. 

library(stringr)
source('Libraries/htcondor-R.R')
source('Libraries/GLM.functions.R')
library(rGLM)

home.dir = getwd()
run.dir = 'D:/WilmaRuns/2014-04-11-0.92perc_increase'
secchi.trend = 0.92
driver.dir = 'D:/WilmaDrivers/07-30'

with.kd = read.table('../supporting files/annual_mean_secchi.txt', header=TRUE, sep='\t')
#with.kd = with.kd[is.na(with.kd$year),]

#runs = paste(run.dir, '/WBIC_', unique(with.kd$WBIC), sep='')
runs = Sys.glob(file.path(run.dir, 'WBIC_*'))
model.ids = basename(runs)
WBICs = str_extract(model.ids,'\\d+')  # WBICS as strings

#filter based on what kd data we have
runs       = runs[as.numeric(WBICs) %in% unique(with.kd$WBIC)]
model.ids  = model.ids[as.numeric(WBICs) %in% unique(with.kd$WBIC)]
WBICs      = WBICs[as.numeric(WBICs) %in% unique(with.kd$WBIC)]

model.files = file.path('D:/WILMA/GLM/1.2',
                        c('glm.exe', 'hdf5_hldll.dll', 'hdf5dll.dll',
                          'libmmd.dll', 'netcdf.dll', 'svml_dispmd.dll', 'szip.dll',
                          'zlib1.dll'))


key.code = file.path(home.dir, c('Libraries/chained.GLM.lib.R'))
key.code = c(key.code, file.path(home.dir, file.path('OnClusterCode',c('run.single.scenario.condor.R',
                                                           'ncdf4_1.4.zip', '.Renviron'))))

key.code = c(key.code, file.path('D:/WILMA/WiLMA-m/R/OnClusterCode', c('rGLM_0.1.5.tar.gz', 'rLakeAnalyzer_1.3.3.tar.gz', 
                                                           'habitat.calc.condor.Kevin.R', '.Renviron', 'ncdf4_1.4.zip', 
                                                           'stringr_0.6.2.zip')))

key.code = c(key.code, file.path('D:/WILMA/WiLMA-m/R/Libraries', c('GLM.physics.R', 'chained.habitat.out.R')))

bat.file = 'D:/WILMA/WiLMA-m/R/OnClusterCode/chainedScenarioHabCalc.bat'


driver.files = paste(file.path(driver.dir,model.ids), '.csv', sep='')


empir.ice = read.table('../supporting files/empirical.ice.tsv', sep='\t', header=TRUE, as.is=TRUE)

for(i in 1:length(runs)){
#for(i in 1:10){
  # Write out ice on/off data for chained runs
  lake.ice.info = empir.ice[empir.ice$WBIC == as.numeric(WBICs[i]), ]
  if(nrow(lake.ice.info) == 0 || all(lake.ice.info$DATE == "")){ #sometimes the lake is in there with empty dates
    next
  }
  write.table(lake.ice.info, file.path(runs[i], 'icecal.in.tsv'), row.names=FALSE, sep='\t')
  
  info = data.frame(WBIC=WBICs[i])
  write.table(info, file.path(runs[i], 'run.info.tsv'), row.names=FALSE)
  
  
  kds = data.frame(year=1979:2011, kd=getScenarioKd(WBICs[i], 1979:2011, trend=secchi.trend))
  write.table(kds, file.path(runs[i], 'kd.scenario.tsv'), row.names=FALSE, sep='\t')
  
  
  #Write the condor submit file with all the appropriate input
  condor.write.submit(file.path(runs[i],'condor.cmd'),
                      executable=bat.file, 
                      c(model.files, driver.files[i], key.code,
                        'glm.nml', 'icecal.in.tsv', 'run.info.tsv', 'kd.scenario.tsv'))
  
  #We need to fix kw values
  kw = getClarity(WBICs[i], default.if.null=TRUE)
  
  #Submit the run!
  setwd(runs[i])
  
  # fix clarity values, default has been updated
  source.nml = read.nml('glm.nml','./')
  source.nml = set.nml(source.nml, 'Kw', kw)
  write.nml(source.nml, 'glm.nml', './')
  
  #Submit
  system('condor_submit condor.cmd')
  setwd(home.dir)
}

system("rundll32 user32.dll,MessageBeep -1")
Sys.sleep(1)
system("rundll32 user32.dll,MessageBeep -1")
Sys.sleep(1)
system("rundll32 user32.dll,MessageBeep -1")

################################################################################
## Just some quick re-run code.
################################################################################

for(i in 1:length(runs)){
  if(!file.exists(file.path(runs[i], 'icecal.in.tsv'))){
    next
  }
  
  if(!file.exists(file.path(runs[i],'habitat.out.tsv'))){
    cat('Resubmitting', runs[i], '\n')
    setwd(runs[i])
    system('condor_submit condor.cmd')
    setwd(home.dir)
  }
}

################################################################################
## Round-up results
################################################################################

fOutput = data.frame()

for(i in 1:length(runs)){
  
  hab.out.path = file.path(runs[i], 'kevin.metrics.out.tsv')
  
  
  if(!file.exists(hab.out.path)){
    next
  }
  
  tmp = read.table(hab.out.path, sep='\t', header=T, as.is=TRUE)
  
  fOutput = rbind(fOutput, tmp)
  
}#WBIC For

system("rundll32 user32.dll,MessageBeep -1")


#Temporary fix. Get rid of proceeding "1" on WBICS
#fOutput$lakeid = str_trim(str_extract(fOutput$lakeid, " ([0-9]+)"))

#Now convert dateOver's to DOY
fOutput$dateOver18 = (as.numeric(fOutput$dateOver18) - as.numeric(as.POSIXct(paste(fOutput$year,'-01-01', sep=''))))/(60*60*24)
#fOutput$dateOver16.7 = (as.numeric(fOutput$dateOver16.7) - as.numeric(as.POSIXct(paste(fOutput$year,'-01-01', sep=''))))/(60*60*24)
fOutput$dateOver21 = (as.numeric(fOutput$dateOver21) - as.numeric(as.POSIXct(paste(fOutput$year,'-01-01', sep=''))))/(60*60*24)
fOutput$dateOver8.9 = (as.numeric(fOutput$dateOver8.9) - as.numeric(as.POSIXct(paste(fOutput$year,'-01-01', sep=''))))/(60*60*24)

if(!file.exists(file.path(run.dir,'output'))){
  dir.create(file.path(run.dir,'output'))  
}

write.table(fOutput, file.path(run.dir, 'output', 'all.kevin.metrics.out.tsv'), 
                               row.names=FALSE, sep='\t')
system("rundll32 user32.dll,MessageBeep -1")


