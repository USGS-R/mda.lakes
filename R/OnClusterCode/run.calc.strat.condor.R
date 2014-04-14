#This file runs accompanies each chained condor run and is run
# at each node.


install.packages("ncdf4_1.4.zip", lib='./rLibs', repos=NULL)
install.packages("rGLM_0.1.5.tar.gz", lib='./rLibs', repos=NULL, type='source')
source('chained.GLM.lib.R')
library(rGLM)

run.prefixed.chained.GLM('.', glm.path = 'glm.exe', NULL, verbose=TRUE)


################################################################################
##Strat Onset
################################################################################
source("GLM.functions.R")
lyrDz  <<-	0.25

calc.strat.onset  <-	function(WBIC,year,min.duration=30,mix.diff=1.0){
  source("GLM.functions.R")
  require("rGLM")
  # min.duration is in TIME UNITS
  # mix diff is °C
  
  file	<-	paste('output',year,'.nc',sep='')
  strat.onset	<-	NA # default is that it was not stratified
  
  # does the file exist?
  if (file %in% list.files()){
    cat('WBIC ');cat(WBIC); cat(' processing.')
    GLMnc  <-	getGLMnc(file,folder='./')
    cat('opened', file, '\n')
    cat('.')
    temp.surf	<-	getTempGLMnc(GLMnc,lyrDz,ref='surface',z.out=0)
    temp.bot	<-	getTempGLMnc(GLMnc,lyrDz,ref='bottom',z.out=0)
    nc_close(GLMnc)
    strat.count	<-	0
    for (j in seq_len(length(temp.surf[, 2]))){
      if (!is.na(temp.surf[j,2]) & !is.na(temp.bot[j,2]) & temp.surf[j,2]-temp.bot[j,2]>mix.diff){
        strat.count = strat.count+1
        if (strat.count>=min.duration){
          strat.onset	<-	as.numeric(strftime(temp.surf[j,1], format = "%j"))-min.duration
          break
        }
      } else {
        strat.count = 0
      }
    }
  }
  return(strat.onset)
}

get.lakes <- function(year){
  info = read.table('run.info.tsv',sep='\t', header=TRUE)
  return(info$WBIC)
}

call.lakes  <-	function(year){
  file.out <- paste("strat.onset",year,'.tsv',sep='')
  lake.ids <- get.lakes(year=year)
  print(lake.ids)
  num.lakes <- length(lake.ids)
  
  strat.matrix <- array(dim=c(num.lakes,1)) #
  
  for (j in 1:num.lakes){
    WBIC = lake.ids[j]
    strat.matrix[j]	<-	calc.strat.onset(WBIC,year)
    cat('.\n')
  }
  
  write.out <- data.frame(lake.ids=lake.ids,strat.matrix)
  # need to name the columns
  col.names	<-	names(write.out)
  col.names	<-	c("WBIC","strat.onset.DoY")
  
  names(write.out) <- col.names
  write.table(x=write.out,file=file.out,sep='\t',col.names=TRUE,quote=FALSE,row.names = FALSE)
}


years = seq(1979,2011,1)
print(years)
for (i in 1:length(years)){
  call.lakes(years[i])
}




################################################################################
##Strat Days
################################################################################

calc.strat.days  <-	function(WBIC,year,min.duration=30,mix.diff=1.0,all.days=1:365){
  # min.duration is in TIME UNITS
  # mix diff is °C
  
  
  file	<-	paste('output',year,'.nc',sep='')
  
  # does the file exist?
  if (file %in% list.files()){
    cat('WBIC ');cat(WBIC); cat(' processing.')
    GLMnc  <-	getGLMnc(file,folder='./')
    cat('.')
    temp.surf	<-	getTempGLMnc(GLMnc,lyrDz,ref='surface',z.out=0)
    temp.bot	<-	getTempGLMnc(GLMnc,lyrDz,ref='bottom',z.out=0)
    nc_close(GLMnc)
    is.strat	<-	(temp.surf[,2]-temp.bot[,2]>mix.diff) # is boolean vector
    strat.DoY	<-	as.numeric(strftime(temp.surf[,1], format = "%j"))
    # get rid of days that aren't stratified
    strat.DoY	<-	strat.DoY[is.strat]
    is.strat	<-	(all.days %in% strat.DoY)
    
    return(is.strat)
  } else {
    return(vector(length=length(all.days)))
  }
}


call.lakes	<-	function(year){
  file.out <- paste("is.strat_",year,'.tsv',sep='')
  lake.ids <- get.lakes(year=year)
  num.lakes <- length(lake.ids)
  all.days=1:365
  strat.count	<-	vector(length=length(all.days))
  
  
  for (j in 1:num.lakes){
    WBIC = lake.ids[j]
    strat.count	<-	calc.strat.days(WBIC,year,all.days=all.days)+strat.count
    cat('.\n')
  }
  
  write.out <- data.frame(DoY=all.days,strat.count=strat.count)
  # need to name the columns
  col.names	<-	names(write.out)
  col.names	<-	c("DoY",paste("strat.count.",num.lakes,sep=''))
  
  names(write.out) <- col.names
  write.table(x=write.out,file=file.out,sep='\t',col.names=TRUE,quote=FALSE,row.names = FALSE)
}
call.lakes(1998)
call.lakes(1996)
