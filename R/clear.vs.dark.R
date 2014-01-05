# find the darkest and clearest quartiles of lakes


dat<-read.table('../supporting files/annual_mean_secchi.txt',header = TRUE, sep = "\t")
source("GLM.functions.R")

all.out <- list.files(path = "../supporting files/10-06Final")
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
clarity.sort

clearest<-  all.out[clarity.sort$ix[1:floor(length(clarity.sort$ix)*.25)]]
darkest <- all.out[clarity.sort$ix[floor(length(clarity.sort$ix)*.75):length(clarity.sort$ix)]]

# --- for each WBIC, find the range (the number to be stored) ---
year.cold <- 1996
year.warm <- 1998
lyrDz = 0.25
file.cold <- paste('output',year.cold,'.nc4',sep='')
file.warm <- paste('output',year.warm,'.nc4',sep='')
require(rGLM)

clearest.range = vector(length=length(clearest))
for (i in 1:length(clearest.range)){
  folder = paste("../supporting files/10-06Final/",clearest[i],'/',sep='')
  GLMnc <- getGLMnc(file.warm,folder=folder)
  temps <- getTempGLMnc(GLMnc,lyrDz)
	nc_close(GLMnc)
  warm.temp <- mean(as.numeric(as.matrix(temps[-1,2:length(names(temps))])),na.rm=TRUE)
  
  GLMnc <- getGLMnc(file.cold,folder=folder)
  temps <- getTempGLMnc(GLMnc,lyrDz)
  cold.temp <- mean(as.numeric(as.matrix(temps[-1,2:length(names(temps))])),na.rm=TRUE)
  
  clearest.range[i] <- warm.temp-cold.temp
}

write.table(x=clearest.range,file="../supporting files/clear.range.tsv",sep='\t',col.names='temp.range')

darkest.range = vector(length=length(darkest))
for (i in 1:length(darkest.range)){
  folder = paste("../supporting files/10-06Final/",darkest[i],'/',sep='')
  GLMnc <- getGLMnc(file.warm,folder=folder)
  temps <- getTempGLMnc(GLMnc,lyrDz)
	nc_close(GLMnc)
  warm.temp <- mean(as.numeric(as.matrix(temps[-1,2:length(names(temps))])),na.rm=TRUE)
  
  GLMnc <- getGLMnc(file.cold,folder=folder)
  temps <- getTempGLMnc(GLMnc,lyrDz)
  cold.temp <- mean(as.numeric(as.matrix(temps[-1,2:length(names(temps))])),na.rm=TRUE)
  darkest.range[i] <- warm.temp-cold.temp
    
}
write.table(x=darkest.range,file="../supporting files/dark.range.tsv",sep='\t',col.names='temp.range')
