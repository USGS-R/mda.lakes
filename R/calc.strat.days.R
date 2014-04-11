source("GLM.functions.R")
require(rGLM)
lyrDz	<<-	0.25
#year	<-	'2009'


calc.strat.days	<-	function(WBIC,year,min.duration=30,mix.diff=1.0,all.days=1:365){
	# min.duration is in TIME UNITS
	# mix diff is °C
	
	folder	<-	paste("../supporting files/10-06Final/WBIC_",WBIC,'/',sep='')
	file	<-	paste('output',year,'.nc4',sep='')
  	
	# does the file exist?
	if (file %in% list.files(path = paste(folder))){
	  	cat('WBIC ');cat(WBIC); cat(' processing.')
	  	GLMnc  <-	getGLMnc(file,folder=folder)
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



get.lakes <- function(year){
  # if doing more than a few years, this should be stripped for just one column and kept in memory (future)
  dat<-read.delim('../supporting files/omg.huge.output.tsv',header = TRUE, sep = "\t")
  # get rid of all rows that don't match the year
  use.idx <- which(dat$year==year & !is.na(dat$dateOver8.9))
  dat <- dat[use.idx,]
  lake.ids <- as.character(dat$lakeid)
  return(lake.ids)
}

call.lakes	<-	function(year){
	file.out <- paste("../supporting files/is.strat_",year,'.tsv',sep='')
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