
library("glmtools")
#year	<-	'2009'


calc.evap	<-	function(WBIC,year){
	# min.duration is in TIME UNITS
	# mix diff is °C
	folder	<-	paste("~/Documents/WiLMA/supporting files/2014-03-13/WBIC_",WBIC,'/',sep='')
	file	<-	paste('output',year,'.nc4',sep='')
	evap	<-	NA # default is that it was not stratified
  	
	# does the file exist?
	if (file %in% list.files(path = paste(folder))){
	  	cat('WBIC ');cat(WBIC); cat(' processing.')
	  	tryCatch({evap = get_evaporation(file = paste0(folder,file))},
	  	         error=function(e){})
	  	cat('.')
      if (any(!is.na(evap))){
        evap	<-	sum(evap[,2], na.rm = T)/1000
      }

	}
	return(evap)
}

#calc.strat.onset('9700',year=1985)
get.lakes <- function(year){
  # if doing more than a few years, this should be stripped for just one column and kept in memory (future)
  dat<-read.delim('~/Documents/WiLMA/supporting files/omg.huge.output.tsv',header = TRUE, sep = "\t")
  # get rid of all rows that don't match the year
  use.idx <- which(dat$year==year & !is.na(dat$dateOver8.9))
  dat <- dat[use.idx,]
  lake.ids <- as.character(dat$lakeid)
  return(lake.ids)
}

call.lakes	<-	function(year){
	file.out <- paste("~/Documents/WiLMA/supporting files/evap",year,'.tsv',sep='')
	lake.ids <- get.lakes(year=year)[1:100]
  lake.ids <- c(lake.ids, '1835300')
	num.lakes <- length(lake.ids)

	evap.matrix <- array(dim=c(num.lakes,1)) #

	for (j in 1:num.lakes){
		WBIC = lake.ids[j]
		
		evap.matrix[j]	<-	calc.evap(WBIC,year)
		cat('.\n')
	}

	write.out <- data.frame(lake.ids=lake.ids,evap.matrix)
	# need to name the columns
	col.names	<-	names(write.out)
	col.names	<-	c("WBIC","evap.mm.day")

	names(write.out) <- col.names
	write.table(x=write.out,file=file.out,sep='\t',col.names=TRUE,quote=FALSE,row.names = FALSE)
}


years = seq(1979,2011,1)
print(years)
for (i in 1:length(years)){
	call.lakes(years[i])
}
