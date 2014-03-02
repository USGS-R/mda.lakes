
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

getRange <- function(vals,len.out=3){
  if (len.out==3){
    #median, 25%, 75%
    df = data.frame("median"=median(vals,na.rm=T),
                    "quart.25"=quantile(x=vals,probs=.25,na.rm=T),
                    "quart.75"=quantile(x=vals,probs=.75,na.rm=T))
  } else if (len.out==2){
    df = data.frame("min"=min(vals,na.rm=T),
                    "max"=max(vals,na.rm=T))
    #min,max
  } else {
    stop('not supported')
  }
  return(df)
}
getZdata <- function(WBICs){
  val = vector(length=length(WBICs))
  for (k in 1:length(val)){
    val[k] <- getZmax(as.character(WBICs[k]))
  }
  
  return(getRange(val,len.out=3))
  
}

getAdata <- function(WBICs){
  val = vector(length=length(WBICs))
  for (k in 1:length(val)){
    val[k] <- getArea(as.character(WBICs[k]))*1.0e-6
  }
  # will be km^2!!

  return(getRange(val,len.out=3))
}

getKdata <- function(WBICs){
  val = vector(length=length(WBICs))
  for (k in 1:length(val)){
    val[k] <- getClarity(as.character(WBICs[k]),default.if.null=FALSE)
  }

  return(getRange(val,len.out=3))
  
}

getHdata <- function(WBICs){
  val = vector(length=length(WBICs))
  for (k in 1:length(val)){
    val[k] <- getCanopy(as.character(WBICs[k]),default.if.null=FALSE)
  }
  
  return(getRange(val,len.out=3))
  
}


getEdata <- function(WBICs){
  val = vector(length=length(WBICs))
  for (k in 1:length(val)){
    val[k] <- getElevation(as.character(WBICs[k]))
  }
  
  return(getRange(val,len.out=2))
  
}

getLatdata <- function(WBICs){
  val = vector(length=length(WBICs))
  for (k in 1:length(val)){
    val[k] <- getLatLon(as.character(WBICs[k]))[1]
  }
  
  return(getRange(val,len.out=2))
  
}
getLondata <- function(WBICs){
  val = vector(length=length(WBICs))
  for (k in 1:length(val)){
    val[k] <- getLatLon(as.character(WBICs[k]))[2]
  }
  
  return(getRange(val,len.out=2))
  
}

getSDFdata <- function(WBICs){
  val = vector(length=length(WBICs))
  for (k in 1:length(val)){
    val[k] <- getSDF(as.character(WBICs[k]))
  }
  
  return(getRange(val,len.out=3))
  
}

printSummary <- function(val,name){
  if (length(val)==3){
    #print 1 (2,3)
    text.1 = formatC(signif(val[1],digits=3), digits=3,format="fg", flag="#")
    text.2 = formatC(signif(val[2],digits=3), digits=3,format="fg", flag="#")
    text.3 = formatC(signif(val[3],digits=3), digits=3,format="fg", flag="#")
    cat(name);cat(" = ")
    cat(text.1);cat(" (")
    cat(text.2);cat(",");cat(text.3);cat(")\n")
  } else if (length(val)==2){
    #print (1,2)
    text.1 = formatC(signif(val[1],digits=5), digits=5,format="fg", flag="#")
    text.2 = formatC(signif(val[2],digits=5), digits=5,format="fg", flag="#")
    cat(name);cat(" = ")
    cat("[")
    cat(text.1);cat(",");cat(text.2);cat("]\n")
  } else {
    text.1 = val
    cat(name);cat(" = ")
    cat(text.1);cat("\n")
    #print val
  }
}
lakes <- getNumLakes()

sim.lakes = list()
val.lakes = list()
sim.lakes$n = length(lakes$sim.lakes)
val.lakes$n = length(lakes$val.lakes)
sim.lakes$Area = getAdata(lakes$sim.lakes)
val.lakes$Area = getAdata(lakes$val.lakes)
sim.lakes$zmax = getZdata(lakes$sim.lakes)
val.lakes$zmax = getZdata(lakes$val.lakes)
sim.lakes$Clarity = getKdata(lakes$sim.lakes)
val.lakes$Clarity = getKdata(lakes$val.lakes)
sim.lakes$h_s = getHdata(lakes$sim.lakes)
val.lakes$h_s = getHdata(lakes$val.lakes)
sim.lakes$SDF = getSDFdata(lakes$sim.lakes)
val.lakes$SDF = getSDFdata(lakes$val.lakes)
sim.lakes$elev = getEdata(lakes$sim.lakes)
val.lakes$elev = getEdata(lakes$val.lakes)
sim.lakes$lat = getLatdata(lakes$sim.lakes)
val.lakes$lat = getLatdata(lakes$val.lakes)
sim.lakes$lon = getLondata(lakes$sim.lakes)
val.lakes$lon = getLondata(lakes$val.lakes)


print('Sim lakes:')
t.names = names(sim.lakes)
ts= sim.lakes
for (i in 1:length(t.names)){
  printSummary(val = unlist(ts[[i]]),name=t.names[i])
}

print('Val lakes:')
t.names = names(val.lakes)
ts= val.lakes
for (i in 1:length(t.names)){
  printSummary(val = unlist(ts[[i]]),name=t.names[i])
}

