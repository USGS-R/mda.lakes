
# build table 3
# error correlation with valuesn VALIDATION LAKES ONLY
# fields: N  A	zmax	Kd	h_s	elev  SDF	lat range	lon range
source('Libraries/GLM.functions.R')
getNumLakes <- function(){
  
  
  Markfort <- read.table(file='../supporting files/summary_hs.1.txt',sep='\t',header=T)
  use.i = !is.na(Markfort$StndErr)
  val.lakes <- Markfort$WBIC[use.i]
  RMSE <- Markfort$StndErr[use.i]
  
  return(data.frame("WBICs"=val.lakes,"RMSE"=RMSE))
  
  
}

getZdata <- function(lakes){
  
  val = vector(length=length(lakes$WBICs))
  for (k in 1:length(val)){
    val[k] <- getZmax(as.character(lakes$WBICs[k]))
  }
  
  # now test pearson
  stt <- cor.test(y=val,x=lakes$RMSE, method = "pearson")
  return(stt)
  # returns median, min and max
  
}

getAdata <- function(lakes){
  val = vector(length=length(lakes$WBICs))
  for (k in 1:length(val)){
    val[k] <- getArea(as.character(lakes$WBICs[k]))*1.0e-6
  }

  # now test pearson
  stt <- cor.test(y=val,x=lakes$RMSE, method = "pearson")
  return(stt)
  
}

getKdata <- function(lakes){
  val = vector(length=length(lakes$WBICs))
  for (k in 1:length(val)){
    val[k] <- getClarity(as.character(lakes$WBICs[k]),default.if.null=FALSE)
  }
  # now test pearson
  stt <- cor.test(y=val,x=lakes$RMSE, method = "pearson")
  return(stt)
  
}

getHdata <- function(lakes){
  val = vector(length=length(lakes$WBICs))
  for (k in 1:length(val)){
    val[k] <- getCanopy(as.character(lakes$WBICs[k]),default.if.null=FALSE)
  }
  
  # now test pearson
  stt <- cor.test(y=val,x=lakes$RMSE, method = "pearson")
  return(stt)
  
  
}


getEdata <- function(lakes){
  val = vector(length=length(lakes$WBICs))
  for (k in 1:length(val)){
    val[k] <- getElevation(as.character(lakes$WBICs[k]))
  }
  
  # now test pearson
  stt <- cor.test(y=val,x=lakes$RMSE, method = "pearson")
  return(stt)
  
}

getLatdata <- function(lakes){
  val = vector(length=length(lakes$WBICs))
  for (k in 1:length(val)){
    val[k] <- getLatLon(as.character(lakes$WBICs[k]))[1]
  }
  
  # now test pearson
  stt <- cor.test(y=val,x=lakes$RMSE, method = "pearson")
  return(stt)
  
  
}
getLondata <- function(WBICs){
  val = vector(length=length(lakes$WBICs))
  for (k in 1:length(val)){
    val[k] <- getLatLon(as.character(lakes$WBICs[k]))[2]
  }
  
  # now test pearson
  stt <- cor.test(y=val,x=lakes$RMSE, method = "pearson")
  return(stt)
  
  
}

getSDFdata <- function(lakes){
  val = vector(length=length(lakes$WBICs))
  for (k in 1:length(val)){
    val[k] <- getSDF(as.character(lakes$WBICs[k]))
  }
  
  # now test pearson
  stt <- cor.test(y=val,x=lakes$RMSE, method = "pearson")
  return(stt)
  
  
}

getRTdata <- function(lakes){
  val = vector(length=length(lakes$WBICs))
  for (k in 1:length(val)){
    val[k] <- getResidenceTime(as.character(lakes$WBICs[k]))
  }
  
  # now test pearson
  stt <- cor.test(y=val,x=lakes$RMSE, method = "pearson")
  return(stt)
  
  
}



printSummary <- function(corrs){
  p.cut.1 <- 0.05
  p.cut.2 <- 0.01
  p.val = corrs$p.value
  correlation <- corrs$estimate
  correlation <- formatC(signif(correlation,digits=3), digits=3,format="fg", flag="#")
  if (p.val<p.cut.2){
    text = paste(correlation,'**',sep='')
  } else if (p.val<p.cut.1){
    text = paste(correlation,'**',sep='')
  } else {
    text = paste(correlation)
  }
  return(text)
}
compareZA <- function(lakes){
  val.x = vector(length=length(lakes$WBICs))
  for (k in 1:length(val.x)){
    val.x[k] <- getArea(as.character(lakes$WBICs[k]))*1.0e-6
  }
  val.y = vector(length=length(lakes$WBICs))
  for (k in 1:length(val.y)){
    val.y[k] <- getZmax(as.character(lakes$WBICs[k]))
  }
  stt <- cor.test(y=val.y,x=val.x, method = "pearson")
  return(stt)
}

compareEL <- function(lakes){
  val.x = vector(length=length(lakes$WBICs))
  for (k in 1:length(val.x)){
    val.x[k] <- getElevation(as.character(lakes$WBICs[k]))*1.0e-6
  }
  val.y = vector(length=length(lakes$WBICs))
  for (k in 1:length(val.y)){
    val.y[k] <- getLatLon(as.character(lakes$WBICs[k]))[1]
  }
  stt <- cor.test(y=val.y,x=val.x, method = "pearson")
  return(stt)
}



lakes <- getNumLakes() #gets all validation lakes with a RMSE value

corrs = list()
corrs$zmax =getZdata(lakes)
corrs$area =getAdata(lakes)
corrs$canopy =getHdata(lakes)
corrs$clarity =getKdata(lakes)
corrs$elevation =getEdata(lakes)
corrs$SDF =getSDFdata(lakes)
corrs$latitude =getLatdata(lakes)
corrs$longitude =getLondata(lakes)
corrs$residenceTime = getRTdata(lakes)
corrs$depth_vs_area =compareZA(lakes)
corrs$elev_vs_late =compareEL(lakes)
cor.nm = names(corrs)
for (i in 1:length(cor.nm)){
  cat(cor.nm[i])
  cat(" =")
  cat(printSummary(corrs[[cor.nm[i]]]))
  cat('\n')
  
}