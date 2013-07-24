#output.cal.chained
#This tries to take calibration data and join it with model output data
# THis particular version deals with the damn chained runs
iscondor = FALSE
if(iscondor){
	install.packages("ncdf4_1.4.zip", lib='./rLibs', repos=NULL)
}

library(ncdf4)
source('GLM.nc.R')

if(!file.exists('cal.in.csv')){
  stop('No Cal Data for this lake')
}

cal.d = read.csv('cal.in.csv', header=TRUE)

nc.files = Sys.glob('output*.nc')
glm.ncs = list()

for(i in 1:length(nc.files)){
	glm.ncs[[i]] = nc_open(nc.files[i])
}

#if(length(ncvar_get(glm.nc,'time')) < 12050){
#  cat('Incomplete Run: Bailing on WBIC:', cal.wbics[i], '\n')
#  next
#}

## Grab just the dates and depths for which we have cal data


## Subsample run to get water temp data at 4D obs points

wtr = getTempGLMnc(glm.ncs[[1]])
for(i in 2:length(glm.ncs)){
	wtr = rbind(wtr, getTempGLMnc(glm.ncs[[i]], lyr.elevations=getElevGLM(wtr)))
}

lake.cal.data = cal.d
lake.cal.dates = unique(lake.cal.data$DATETIME)
lake.cal.depths = sort(unique(lake.cal.data$DEPTH))

lake.mod.data = data.frame(WBIC=lake.cal.data$WBIC, 
                           DATETIME=lake.cal.data$DATETIME, 
                           DEPTH=lake.cal.data$DEPTH, 
                           WTEMP=lake.cal.data$DEPTH*NaN)

lake.cal.data$WTEMP_MOD = NaN

tmp = subsampleGLM(wtr, lake.cal.dates, sort(lake.cal.depths))

depthLookup = match(lake.cal.data$DEPTH, lake.cal.depths)
datesLookup = match(lake.cal.data$DATETIME, lake.cal.dates)


for(j in 1:nrow(lake.cal.data)){
  lake.cal.data$WTEMP_MOD[j] = tmp[datesLookup[j], depthLookup[j]]
}

#out.fname = paste(runs.dir, '/WBIC_', cal.wbics[i], '/cal.csv', sep='')
write.csv(lake.cal.data, 'cal.out.csv', row.names=FALSE)

for(i in 1:length(glm.ncs)){
	nc_close(glm.ncs[[i]])
}

