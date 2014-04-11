source("Libraries/GLM.functions.R")
require("rGLM")
lyrDz	<<-	0.25
#year	<-	'2009'


calc.strat.onset	<-	function(WBIC,year,min.duration=30,mix.diff=1.0){
	source("Libraries/GLM.functions.R")
	require("rGLM")
	# min.duration is in TIME UNITS
	# mix diff is °C
	folder	<-	paste("../supporting files/10-06Final/WBIC_",WBIC,'/',sep='')
	file	<-	paste('output',year,'.nc4',sep='')
	strat.onset	<-	NA # default is that it was not stratified
  	
	# does the file exist?
	if (file %in% list.files(path = paste(folder))){
	  	cat('WBIC ');cat(WBIC); cat(' processing.')
	  	GLMnc  <-	getGLMnc(file,folder=folder)
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

#calc.strat.onset('9700',year=1985)
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
	file.out <- paste("../supporting files/strat.onset",year,'.tsv',sep='')
	lake.ids <- get.lakes(year=year)
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
