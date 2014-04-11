## Run all condor
## This submit file creates condor submit files for workflow that
## does a few things. 

library(stringr)
source('Libraries/htcondor-R.R')

home.dir = getwd()
run.dir = 'D:/WilmaRuns/2014-03-13-2'
driver.dir = 'D:/WilmaDrivers/07-30'

#run.dir = 'D:/WilmaLTER'
#driver.dir = 'D:/WilmaLTER/Driver files'

runs = Sys.glob(file.path(run.dir, 'WBIC_*'))
model.ids = basename(runs)
WBICs = str_extract(model.ids,'\\d+')  # WBICS as strings

model.files = file.path('D:/WILMA/GLM/1.2', 
                        c('glm.exe', 'hdf5_hldll.dll', 'hdf5dll.dll',
                          'libmmd.dll', 'netcdf.dll', 'svml_dispmd.dll', 'szip.dll',
                          'zlib1.dll'))
nc.files = file.path('D:/WILMA/WILMA-m/R/CompressCode', 
                     c('netcdfBins.zip', 'bzip2.dll', 
                       'unzip.exe', 'zip.exe', 'zip32z64.dll'))


R.code = file.path(home.dir, c('Libraries/chained.GLM.lib.R', '3dayPrefix.csv'))
R.code.2 = file.path(home.dir, file.path('OnClusterCode',c('run.single.condor.R','rGLM_0.1.5.tar.gz',
                                                           'ncdf4_1.4.zip', '.Renviron')))


bat.file = 'D:/WILMA/WiLMA-m/R/OnClusterCode/chainedModelNcCompress.bat'


driver.files = paste(file.path(driver.dir,model.ids), '.csv', sep='')

#fig.gen.files = file.path('D:/WILMA/WILMA-R/CreateFiguresCondor',
#                c('create.glm.figures.R','GLMnetCDF.R','GlmPhysicalDerivatives.r','.Renviron','ncdf4_1.4.zip'))

empir.ice = read.table('../supporting files/empirical.ice.tsv', sep='\t', header=TRUE, as.is=TRUE)

for(i in 1:length(runs)){
#for(i in 1:10){
  
  # Write out ice on/off data for chained runs
  lake.ice.info = empir.ice[empir.ice$WBIC == as.numeric(WBICs[i]), ]
  if(nrow(lake.ice.info) == 0){
    next
  }
  write.table(lake.ice.info, file.path(runs[i], 'icecal.in.tsv'), row.names=FALSE, sep='\t')
  
  
  #Write the condor submit file with all the appropriate input
  condor.write.submit(file.path(runs[i],'condor.cmd'),
                      executable=bat.file, 
                      c(model.files, driver.files[i], R.code,
                        R.code.2, nc.files, 'glm.nml', 'icecal.in.tsv'))
  
  #Submit the run!
  setwd(runs[i])
  system('condor_submit condor.cmd')
  setwd(home.dir)
}

##

#Ok, once we've submitted and run all models, we want to figure out how
# many failed and by how much to change the precip

failed = c()

for(i in 1:length(runs)){
  #Open lake.csv
  
  info = file.info(file.path(runs[i], 'lake.csv'))
  if(is.na(info$size) || info$size == 0){
    failed = c(failed, runs[i])
    next
  }
  
  d = read.table(file.path(runs[i], 'lake.csv'),
                 header=TRUE, as.is=TRUE, sep=',')
  
  #Get Length
  overflow.vol = d$Overflow.Vol
  
  #Too short means failed
  if(length(overflow.vol) < 12050){
    failed = c(failed, runs[i])
  }
  
  #Either way, change the toAdd value based on overflow
  
  #math = read.table(file.path(runs[i], "Math.csv.orig"), 
  #                  header=TRUE, as.is=TRUE, sep=',')
  
  #math$toAdd = math$toAdd - 
  #              sum(overflow.vol)/(math$Area/1000)/length(overflow.vol)
  #12050 * 24 is the timestep number
  #math$toAdd = math$toAdd - 
  #              sum(overflow.vol)/(length(overflow.vol))
  
  #rename old math file so we don't screw this up
  file.copy(file.path(runs[i], 'Math.csv.orig'),
             file.path(runs[i], 'Math.csv'), overwrite=TRUE)
  
  
  #rewite the updated math file
  #write.table(math, file.path(runs[i], 'Math.csv'), sep=',', col.names=TRUE, row.names=FALSE)
  setwd(runs[i])
  system('condor_submit condor.cmd')
  setwd(home.dir)
  
}

## Resubmit!!
for(i in 1:length(runs)){
    
  setwd(runs[i])
  system('condor_submit condor.cmd')
  setwd(home.dir)
}


#How many failed?
failed = c()

for(i in 1:length(runs)){
  
  #Every once in a while, we get an empty lake.csv
  info = file.info(file.path(runs[i], 'lake.csv'))
  if(info$size == 0){
    failed = c(failed, runs[i])
    next
  }
  
  #Open lake.csv
  d = read.table(file.path(runs[i], 'lake.csv'),
                 header=TRUE, as.is=TRUE, sep=',')
  
  #Get Length
  overflow.vol = d$Overflow.Vol
  
  #cat(length(overflow.vol))
  cat(i,"\n")
  
  
  #Too short means failed
  if(length(overflow.vol) < 12050){
    failed = c(failed, runs[i])
  }
  
}

