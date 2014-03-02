
# build table 2
# fields: N  A	zmax	Kd	h_s	elev  SDF	lat range	lon range
source('GLM.functions.R')
getNumLakes <- function(){
  
  sens.table  <-	read.delim("../supporting files/omg.huge.output.tsv",sep='\t',header=T)
  #use.i = sens.table$year==1979
  sim.WBICs = sens.table$lakeid#[use.i]
  sim.lakes = unique(sim.WBICs)
  
  Markfort <- read.table(file='../supporting files/summary_hs.1.txt',sep='\t',header=T)
  use.i = Markfort$numPoints!=0
  val.lakes <- Markfort$WBIC[use.i]
  
  return(list("sim.lakes"=sim.lakes,"val.lakes"=val.lakes))
  
  
}

getZdata <- function(WBICs){
  zmax = vector(length=length(WBICs))
  for (k in 1:length(zmax)){
    zmax[k] <- getZmax(as.character(WBICs[k]))
  }
  
  print(WBICs[which.max(zmax)])
  return(data.frame("median"=median(zmax),"min"=min(zmax),"max"=max(zmax)))
  # returns median, min and max
  
}

getAdata <- function(WBICs){
  A = vector(length=length(WBICs))
  for (k in 1:length(A)){
    A[k] <- getArea(as.character(WBICs[k]))*1.0e-6
  }
  # will be km^2!!
  print(WBICs[which.max(A)])
  plot(A)
  return(data.frame("median"=median(A),"min"=min(A),"max"=max(A)))
  # returns median, min and max
  
}

getKdata <- function(WBICs){
  Kd = vector(length=length(WBICs))
  for (k in 1:length(Kd)){
    Kd[k] <- getClarity(as.character(WBICs[k]),default.if.null=FALSE)
  }

  return(data.frame("median"=median(Kd,na.rm=T),"min"=min(Kd,na.rm=T),"max"=max(Kd,na.rm=T)))
  # returns median, min and max
  
}

getHdata <- function(WBICs){
  h_s = vector(length=length(WBICs))
  for (k in 1:length(h_s)){
    h_s[k] <- getCanopy(as.character(WBICs[k]),default.if.null=FALSE)
  }
  
  return(data.frame("median"=median(h_s,na.rm=T),"min"=min(h_s,na.rm=T),"max"=max(h_s,na.rm=T)))
  # returns median, min and max
  
}


getEdata <- function(WBICs){
  val = vector(length=length(WBICs))
  for (k in 1:length(val)){
    val[k] <- getElevation(as.character(WBICs[k]))
  }
  
  return(data.frame("median"=median(val,na.rm=T),"min"=min(val,na.rm=T),"max"=max(val,na.rm=T)))
  # returns median, min and max
  
}

getLatdata <- function(WBICs){
  val = vector(length=length(WBICs))
  for (k in 1:length(val)){
    val[k] <- getLatLon(as.character(WBICs[k]))[1]
  }
  
  return(data.frame("median"=median(val,na.rm=T),"min"=min(val,na.rm=T),"max"=max(val,na.rm=T)))
  # returns median, min and max
  
}
getLondata <- function(WBICs){
  val = vector(length=length(WBICs))
  for (k in 1:length(val)){
    val[k] <- getLatLon(as.character(WBICs[k]))[2]
  }
  
  return(data.frame("median"=median(val,na.rm=T),"min"=min(val,na.rm=T),"max"=max(val,na.rm=T)))
  # returns median, min and max
  
}

getSDFdata <- function(WBICs){
  val = vector(length=length(WBICs))
  for (k in 1:length(val)){
    val[k] <- getSDF(as.character(WBICs[k]))
  }
  
  return(data.frame("median"=median(val,na.rm=T),"min"=min(val,na.rm=T),"max"=max(val,na.rm=T)))
  # returns median, min and max
  
}

lakes <- getNumLakes()
print(paste('sim lakes n=',length(lakes$sim.lakes)),sep='')
print(paste('val lakes n=',length(lakes$val.lakes)),sep='')
zmax.val = getZdata(lakes$val.lakes)
print(zmax.val)
zmax.sim = getZdata(lakes$sim.lakes)
print(zmax.sim)

A.sim = getAdata(lakes$sim.lakes)
A.val = getAdata(lakes$val.lakes)

Kd.sim = getKdata(lakes$sim.lakes)
Kd.val = getKdata(lakes$val.lakes)

hs.sim = getHdata(lakes$sim.lakes)
hs.val = getHdata(lakes$val.lakes)
  
elev.sim = getEdata(lakes$sim.lakes)
elev.val = getEdata(lakes$val.lakes)

lat.sim = getLatdata(lakes$sim.lakes)
lat.val = getLatdata(lakes$val.lakes)

lon.sim = getLondata(lakes$sim.lakes)
lon.val = getLondata(lakes$val.lakes)

sdf.sim = getSDFdata(lakes$sim.lakes)
sdf.val = getSDFdata(lakes$val.lakes)
