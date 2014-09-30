# find the darkest and clearest quartiles of lakes

source("Libraries/GLM.functions.R")
require(rGLM)
years	<-	seq(1979,2011,1)
lyrDz = 0.25
all.out <- list.files(path = "../supporting files/10-06Final")

dat<-read.table('../supporting files/annual_mean_secchi.txt',header = TRUE, sep = "\t")


clarity <- vector(length=length(all.out))

for (i in 1:length(clarity)){
  WBIC = all.out[i]
  kd <- getClarity(strsplit(x=WBIC,"_")[[1]][2])
  if (is.null(kd)){
    kd = NA
  }
  clarity[i] <- kd
}

# remove all NAs
all.out = all.out[!is.na(clarity)]
clarity = clarity[!is.na(clarity)]

clarity.sort <- sort(clarity,index.return=TRUE)

# sorted clearest to darkest

clearest<-  all.out[clarity.sort$ix[1:floor(length(clarity.sort$ix)*.25)]]
darkest <- all.out[clarity.sort$ix[floor(length(clarity.sort$ix)*.75):length(clarity.sort$ix)]]

# --- for each WBIC, find the range (the number to be stored) ---



clearest.temps = array(dim=c(length(clearest),length(years)))
for (i in 1:length(clearest)){
	folder = paste("../supporting files/10-06Final/",clearest[i],'/',sep='')
	for (j in 1:length(years)){
		file.name	<- paste('output',years[j],'.nc4',sep='')
		if (file.name %in% list.files(path = paste(folder))){
			GLMnc <- getGLMnc(file.name,folder=folder)
			temps <- getTempGLMnc(GLMnc,lyrDz)
			nc_close(GLMnc)
			clearest.temps[i,j]	<-	mean(as.numeric(as.matrix(temps[-1,2:length(names(temps))])),na.rm=TRUE)
		} else {
			print(paste('skipping ',clearest[i],file.name,sep=''))
			clearest.temps[i,j]	<-	NA
		}
		cat("i=");cat(i);cat(", j=");cat(j);cat('\n')
	}
}

write.clear	<-	data.frame(WBIC=clearest)
write.clear	<-	cbind(write.clear,data.frame(clearest.temps))
names(write.clear)	<-	c('WBIC',as.character(years))

write.table(x=write.clear,file="../supporting files/clear.range.tsv",sep='\t',col.names=TRUE,row.names = FALSE,quote=FALSE)

darkest.temps	<-	array(dim=c(length(darkest),length(years)))
for (i in 1:length(darkest)){
	folder = paste("../supporting files/10-06Final/",darkest[i],'/',sep='')
	for (j in 1:length(years)){
		file.name	<- paste('output',years[j],'.nc4',sep='')
		if (file.name %in% list.files(path = paste(folder))){
			GLMnc <- getGLMnc(file.name,folder=folder)
			temps <- getTempGLMnc(GLMnc,lyrDz)
			nc_close(GLMnc)
			darkest.temps[i,j]	<-	mean(as.numeric(as.matrix(temps[-1,2:length(names(temps))])),na.rm=TRUE)
		} else {
			print(paste('skipping ',darkest[i],file.name,sep=''))
			darkest.temps[i,j]	<-	NA
		}
		cat("i=");cat(i);cat(", j=");cat(j);cat('\n')
	}
}
write.dark	<-	data.frame(WBIC=darkest)
write.dark	<-	cbind(write.dark,data.frame(darkest.temps))
names(write.dark)	<-	c('WBIC',as.character(years))

write.table(x=write.dark,file="../supporting files/dark.range.tsv",sep='\t',col.names=TRUE,row.names = FALSE,quote=FALSE)
