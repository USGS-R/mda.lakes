## Run all condor
## This submit file creates condor submit files for workflow that
## does a few things. 

library(stringr)
source('Libraries/htcondor-R.R')
source('Libraries/GLM.functions.R')
library(rGLM)

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


R.code = file.path(home.dir, c('cLibraries/hained.GLM.lib.R', '3dayPrefix.csv', 'Libraries/GLM.functions.R'))
R.code.2 = file.path(home.dir, file.path('OnClusterCode',c('run.single.condor.R','rGLM_0.1.5.tar.gz',
                                                           'ncdf4_1.4.zip', '.Renviron',
                                                           'run.calc.strat.condor.R')))


bat.file = 'D:/WILMA/WiLMA-m/R/OnClusterCode/chainedModelStratCalc.bat'


driver.files = paste(file.path(driver.dir,model.ids), '.csv', sep='')

#fig.gen.files = file.path('D:/WILMA/WILMA-R/CreateFiguresCondor',
#                c('create.glm.figures.R','GLMnetCDF.R','GlmPhysicalDerivatives.r','.Renviron','ncdf4_1.4.zip'))

empir.ice = read.table('../supporting files/empirical.ice.tsv', sep='\t', header=TRUE, as.is=TRUE)

for(i in 1:length(runs)){
  
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
                        R.code.2, nc.files, 'glm.nml', 'icecal.in.tsv','run.info.tsv'))
  
  #We need to fix kw values
  kw = getClarity(WBICs[i], default.if.null=TRUE)
  
  #Submit the run!
  setwd(runs[i])
  
  # fix clarity values, default has been updated
  source.nml = read.nml('glm.nml','./')
  set.nml(source.nml, 'Kw', kw)
  write.nml(source.nml, 'glm.nml', './')
  
  #Write run info
  write.table(data.frame(WBIC=WBICs[i]), 'run.info.tsv', row.names=FALSE, sep='\t')
  
  #Submit
  system('condor_submit condor.cmd')
  setwd(home.dir)
}

################################################################################
## Just some quick re-run code.
################################################################################

for(i in 1:length(runs)){
  if(!file.exists(file.path(runs[i], 'icecal.in.tsv'))){
    next
  }
  
  if(!file.exists(file.path(runs[i],'lake.csv'))){
    cat('Resubmitting', runs[i], '\n')
    setwd(runs[i])
    system('condor_submit condor.cmd')
    setwd(home.dir)
  }
}



#Ok, once we've submitted and run all models, we want to figure out how
# many failed and by how much to change the precip

comb.is.strat = function(path, years, is.strat, n.year){
  for(i in 1:length(years)){
    fname = paste(path, '/', 'is.strat_', years[i], '.tsv', sep='')
    
    if(!file.exists(fname)){
      next
    }
    tmp = read.table(fname, header=TRUE)
    is.strat[[years[i]]] = is.strat[[years[i]]] + tmp$strat.count.1
    n.year[[years[i]]] = n.year[[years[i]]] + 1
    cat('.')
  }
  return(list(is.strat, n.year))
}


is.strat = list()
is.strat[c(1996,1998)] = 0

n.year <<- list()
n.year[c(1996,1998)] = 0


for(i in 1:length(runs)){
  #Open lake.csv
  cat('running', runs[i])
  tmp = comb.is.strat(runs[i], c(1996,1998), is.strat, n.year)
  is.strat = tmp[[1]]
  n.year = tmp[[2]]
  cat('\n')
}

write.table(
  data.frame(DoY=1:365, strat.count.1 = is.strat[[1996]]),
  'is.strat_1996.tsv', row.names=FALSE, sep='\t')

write.table(
  data.frame(DoY=1:365, strat.count.1 = is.strat[[1998]]),
  'is.strat_1998.tsv', row.names=FALSE, sep='\t')



comb.strat.onset = function(path, years, strat.onset){
  for(i in 1:length(years)){
    fname = paste(path, '/', 'strat.onset', years[i], '.tsv', sep='')
    
    if(!file.exists(fname)){
      next
    }
    tmp = read.table(fname, header=TRUE, sep='\t')
    strat.onset[[years[i]]] = rbind(strat.onset[[years[i]]], tmp)
    cat('.')
  }
  return(strat.onset)
}



strat.onset = list()
for(y in seq(1979,2011,1)){
strat.onset[[y]] = data.frame()
}
for(i in 1:length(runs)){
  #Open lake.csv
  cat('running', runs[i])
  strat.onset = comb.strat.onset(runs[i], seq(1979,2011,1), strat.onset)
  cat('\n')
}

for(y in seq(1979,2011,1)){
   write.table(strat.onset[[y]],
               paste('strat.onset', y, '.tsv', sep=''),
               row.names=FALSE, sep='\t')
}




