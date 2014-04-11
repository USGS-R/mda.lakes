################################################################################
## Run all the habitat calc code
################################################################################
## Luke Winslow <lawinslow@wisc.edu>
################################################################################

#Argh, so display isn't off an hour in the winter (stupid daylight savings)
Sys.setenv(TZ='UTC')

#Source the library for pulling data from NC files
library(stringr)
source('htcondor-R.R')


#these are the wibcs to look for first
#wbics = read.table('../April2012toAnalyze.csv',header=TRUE)

run.dir = 'D:/WilmaRuns/08-09Final'
home.dir = getwd()

folders = Sys.glob(file.path(run.dir,'WBIC_*'))

wbics = str_extract(basename(folders),"[0-9]+")



bat.file = 'D:/WILMA/WiLMA-m/R/OnClusterCode/habitatcalc.bat'

key.code = file.path('D:/WILMA/WiLMA-m/R/OnClusterCode', c('rGLM_0.1.2.tar.gz', 'rLakeAnalyzer_1.2.zip', 
                                                           'habitat.calc.condor.R', '.Renviron', 'ncdf4_1.4.zip', 
                                                           'stringr_0.6.2.zip'))

key.code = c(key.code, file.path('D:/WILMA/WiLMA-m/R', c('GLM.physics.R', 'chained.habitat.out.R')))

for(i in 1:length(wbics)){
  
  nc.files = Sys.glob(file.path(folders[i], '*.nc'))
  
  if(length(nc.files) > 0){
    info = data.frame(WBIC=wbics[i])
    write.table(info, file.path(folders[i], 'run.info.tsv'))
    
    file.copy(bat.file, file.path(folders[i], 'habitatcalc.bat'),
              overwrite=TRUE)
    
    condor.write.submit(file.path(folders[i],'condor.cmd'),
                        executable='habitatcalc.bat', 
                        c('run.info.tsv', key.code, nc.files))
    
    
    setwd(folders[i])
    system('condor_submit condor.cmd')
    setwd(home.dir)
    
  }#ncFiles exist IF
  
}#WBIC For



## Round-up results

fOutput = data.frame()

for(i in 1:length(folders)){
  
  hab.out.path = file.path(folders[i], 'habitat.out.tsv')
  
  if(!file.exists(hab.out.path)){
    next
  }
  
  tmp = read.table(hab.out.path, sep='\t', header=T, as.is=TRUE)
  
  fOutput = rbind(fOutput, tmp)
    
}#WBIC For

#Temporary fix. Get rid of proceeding "1" on WBICS
fOutput$lakeid = str_trim(str_extract(fOutput$lakeid, " ([0-9]+)"))

#Now convert dateOver's to DOY
fOutput$dateOver18 = (as.numeric(fOutput$dateOver18) - as.numeric(as.POSIXct(paste(fOutput$year,'-01-01', sep=''))))/(60*60*24)
fOutput$dateOver16.7 = (as.numeric(fOutput$dateOver16.7) - as.numeric(as.POSIXct(paste(fOutput$year,'-01-01', sep=''))))/(60*60*24)
fOutput$dateOver21 = (as.numeric(fOutput$dateOver21) - as.numeric(as.POSIXct(paste(fOutput$year,'-01-01', sep=''))))/(60*60*24)
fOutput$dateOver8.9 = (as.numeric(fOutput$dateOver8.9) - as.numeric(as.POSIXct(paste(fOutput$year,'-01-01', sep=''))))/(60*60*24)


write.table(fOutput,'omg.huge.output.tsv', row.names=FALSE, sep='\t')


