# this takes a single lake WBIC, a simulation year, an array of temperature errors (to be randomly sampled from), and the number of realizations

source("GLM.functions.R")
require(rGLM)
nc.dir	<-	"../supporting files/10-06Final"
val.file	<-	"../supporting files/Epilimnion.tsv"

year<- 2009
lyrDz	<-	0.25
threshold <- 8.9
error.range <- c(4,45)
quantiles <- c(0.025,0.25,0.5,0.75,0.975)
n=10000
file.out <- paste("../supporting files/exceed_",threshold,"C_errors_",year,'.tsv',sep='')

get.lakes <- function(year){
  # if doing more than a few years, this should be stripped for just one column and kept in memory (future)
  dat<-read.delim('../supporting files/omg.huge.output.tsv',header = TRUE, sep = "\t")
  # get rid of all rows that don't match the year
  use.idx <- which(dat$year==year & !is.na(dat$dateOver8.9))
  dat <- dat[use.idx,]
  lake.ids <- as.character(dat$lakeid)
  return(lake.ids)
}

get.errors	<-	function(file=val.file,error.range=c(7.14,10.66),n=10000){
	# truncate according to range in OBSERVATIONAL values
  dat   <- read.delim(file)
  use.idx <- which((dat$observed>=error.range[1]) & (dat$observed<=error.range[2]))
  errors <-  dat$observed[use.idx]-dat$modeled[use.idx]
  resampled.errors <- sample(errors, size=n, replace = TRUE)
  errors <- as.numeric(quantile(resampled.errors,quantiles))
  return(errors)
}

get.DoY.exceed  <-	function(temps,threshold=8.9,start.DoY=1){
  # convert dates to DoY (assumes no duplication of DoYs, i.e., single year)
  dates	<-	temps$DateTime
  DoY	<- as.numeric(strftime(dates, format = "%j"))
  exceed.idx <- which(temps$wtr_0>=threshold)
  if (length(exceed.idx)==0){
    return(DoY=NA)
  } else {
    first.exceed <- min(exceed.idx)
    return(DoY[first.exceed])
  }
}

random.error.temp	<-	function(WBIC,year,errors){
	# do not assume the lake is there or that it is complete 
  # errors is now specific adjustment errors
	folder	<-	paste("../supporting files/10-06Final/WBIC_",WBIC,'/',sep='')
	file	<-	paste('output',year,'.nc4',sep='')
  # does the file exist?
	if (file %in% list.files(path = paste(folder))){
	  cat('WBIC ');cat(WBIC); cat(' processing.')
	  GLMnc  <-	getGLMnc(file,folder=folder)
	  cat('.')
	  temps	<-	getTempGLMnc(GLMnc,lyrDz,ref='surface',z.out=0)
	  nc_close(GLMnc)
	  
	  DoY <- vector(length=length(errors))
	  for (i in 1:length(errors)){
	    DoY[i] <- get.DoY.exceed(temps,threshold=threshold+errors[i]) # check: is this supposed to be + or -?
	  }
	  cat('.\n')
	  return(DoY)
	} else {
    return(FALSE)
	}
	
  
}

lake.ids <- get.lakes(year=year)
num.lakes <- length(lake.ids)
lake.ids <- lake.ids
active.lakes <- vector(length=num.lakes)
exceed.matrix <- array(dim=c(num.lakes,length(quantiles))) # contains 3 quantiles for parameter exceedence 
temp.errors  <-  get.errors(file=val.file,error.range=error.range) # only once!

for (j in 1:num.lakes){
  DoY <- random.error.temp(lake.ids[j],year,errors=temp.errors)
  if (any(is.na(DoY))){DoY=FALSE}
  if (!DoY[1]){
    # active.lakes will remain FALSE
    cat('WBIC ')
    cat(lake.ids[j])
    cat(' is missing\n')
  } else {
    exceed.matrix[j,] <- DoY
    cat('done with WBIC ')
    cat(lake.ids[j])
    cat('\n')
    active.lakes[j] = TRUE
  }
  
}

exceed.matrix = exceed.matrix[active.lakes,]
lake.ids = lake.ids[active.lakes]

write.out <- data.frame(lake.ids=lake.ids,exceed.matrix)
# need to name the columns
col.names <- names(write.out)
col.names[1] <- "WBIC"
for (k in 1:length(quantiles)){
  quant.name <- paste("exceed",threshold,'°C(',quantiles[k]*100,'%)',sep='')
  col.names[k+1] <- quant.name
}
names(write.out) <- col.names
write.table(x=write.out,file=file.out,sep='\t',col.names=TRUE,quote=FALSE,row.names = FALSE)
