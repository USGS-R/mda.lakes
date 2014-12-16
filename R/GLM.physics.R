# fish code for WiLMA 
# **jread-usgs, lawinslow 2013-04-07



# -- shared variables --
timeID  <-  "DateTime"
iceID <-  "ice.m."

################################################################################
#
################################################################################
subsetTime <- function(GLM,startDate,stopDate){
  # gets rid of GLM simulation results that are < startDate and > stopDate
  # startDate and stopDate must be of type "character"
  dates <-  GLM$DateTime
  
  if(!inherits(startDate,"POSIXt")){
    startDate = as.POSIXct(startDate)
  }
  if(!inherits(stopDate,"POSIXt")){
    stopDate = as.POSIXct(stopDate)
  }
  
  useI  <-  (dates >= startDate) & (dates <= stopDate)
  GLM <-  GLM[useI,]
  return(GLM)
}

################################################################################
#
################################################################################
getTemp   <- function(GLMwtr){
  drops <- c(timeID)
  temp <- GLMwtr[,!(names(GLMwtr) %in% drops)]
  return(temp)
}

################################################################################
#
################################################################################
getDailyTempMax <- function(GLMwtr){
  temp <- getTemp(GLMwtr)
  dailyTempMax <-  apply(temp,1,function(x) max(x,na.rm=TRUE))
  return(dailyTempMax)
}

################################################################################
#
################################################################################
getDailyTempMin <- function(GLMwtr){
  temp <- getTemp(GLMwtr)
  dailyTempMin <-  apply(temp,1,function(x) min(x,na.rm=TRUE))
  return(dailyTempMin)
}

################################################################################
#
################################################################################
getTempMax <- function(GLMwtr){
  dailyTempMax <-  getDailyTempMax(GLMwtr)
  tempMax <-  max(dailyTempMax)
  return(tempMax)
}

################################################################################
#
################################################################################
getTempMin <- function(GLMwtr){
  dailyTempMin <-  getDailyTempMin(GLMwtr)
  tempMin <-  min(dailyTempMin)
  return(tempMin)
}

################################################################################
#
################################################################################
getDaysAboveT <- function(GLMwtr,temperature,anyDep=TRUE){
  # ANY or ALL depths, default is ANY
  if (anyDep==TRUE){
    refTemp  <-  getDailyTempMax(GLMwtr)
  }else {
    refTemp <-  getDailyTempMin(GLMwtr)
  }

  tempAboveCount  <-  sum(refTemp > temperature,na.rm=TRUE)
  return(tempAboveCount)
}

################################################################################
#
################################################################################
getDaysBelowT <- function(GLMwtr,temperature,anyDep=TRUE){
  # ANY or ALL depths, default is ANY
  temp <- getTemp(GLMwtr)
  if (anyDep==TRUE){
    refTemp  <-  getDailyTempMin(GLMwtr)
  }else {
    refTemp <-  getDailyTempMax(GLM)
  }
  
  tempBelowCount  <-  sum(refTemp < temperature,na.rm=TRUE)
  return(tempBelowCount)
}

################################################################################
#
################################################################################
getDaysBetweenT <-  function(GLMwtr,temperatureLow,temperatureHigh,anyDep=TRUE){
  # ANY or ALL depths, default is ANY
  temp <- getTemp(GLMwtr)
  if (anyDep==TRUE){
    tempRangeCount  <-  sum(apply(temp,1,function(x) any(x>=temperatureLow,na.rm=TRUE) & any(x<=temperatureHigh,na.rm=TRUE)))
  }else{
    tempRangeCount  <-  sum(apply(temp,1,function(x) all(x>=temperatureLow,na.rm=TRUE) 
      & all(x<=temperatureHigh,na.rm=TRUE)))
  }
  return(tempRangeCount)
}

################################################################################
#
################################################################################
getMaxTempIdx <-  function(GLMwtr){
  dailyTempMax  <-  getDailyTempMax(GLMwtr)
  maxTempIdx  <-  which.max(dailyTempMax)
  return(maxTempIdx)
}

################################################################################
#
################################################################################
getSurfaceT <- function(GLMwtr){
  temp <- getTemp(GLMwtr)
  surfaceTemp <- apply(temp,1,function(x) x[max(which(!is.na(x)))])
  return(surfaceTemp)
}

################################################################################
#
################################################################################
getBottomT <- function(GLMwtr){
  temp <- getTemp(GLMwtr)
  bottomTemp <- apply(temp,1,function(x) x[min(which(!is.na(x)))])
  return(bottomTemp)
}

################################################################################
#
################################################################################
getIceOffDate <- function(GLMice,GLMwtr){
  if(diff(range(GLMwtr$DateTime)) > as.difftime(366,units="days")){
    stop("GLM ice time series must be equal or shorter than one year")
  }
  
  maxTempIdx <-  as.numeric(getMaxTempIdx(GLMwtr))
  
  # now, look backwards
  nonzero.ice.idx = which(GLMice[iceID][1:maxTempIdx,]!=0)
  
  # if we never had ice cover from start of sim, just set to zero
  iceOffIdx <- max(ifelse(length(nonzero.ice.idx) < 1, 0, nonzero.ice.idx)) + 1
  iceOffDOY <-  GLMice[timeID][iceOffIdx,]
  return(iceOffDOY)
}

################################################################################
#
################################################################################
getIceOnDate  <-  function(GLMice,GLMwtr){
  if(diff(range(GLMwtr$DateTime)) > as.difftime(366,units="days")){
    stop("GLM ice time series must be equal or shorter than one year")
  }
  maxTempIdx <-  as.numeric(getMaxTempIdx(GLMwtr))
    # now, look forwards
  iceOnIdx <-  min(which(GLMice[iceID][maxTempIdx:nrow(GLMice[iceID]),]!=0))+(maxTempIdx-1)
  iceOnDOY <-  GLMice[timeID][iceOnIdx,]
  
  return(iceOnDOY)
}

################################################################################
#
################################################################################
getLastDayAboveT <-  function(GLMwtr,temperature,anyDep=TRUE){
  if(diff(range(GLMwtr$DateTime)) > as.difftime(366,units="days")){
    stop("GLM ice time series must be equal or shorter than one year")
  }
  if (anyDep==TRUE){
    tempRef <-  getDailyTempMax(GLMwtr)
    lastIdx <-  max(which(tempRef>temperature))
  }
  else{
    tempRef <-  getDailyTempMin(GLMwtr)
    lastIdx <-  max(which(tempRef>temperature))
  }
  lastDOYabove  <-  GLMwtr[timeID][lastIdx,]
  return(lastDOYabove)
}

################################################################################
getFirstDayAboveT <-  function(GLMwtr,temperature,anyDep=TRUE){
  if(diff(range(GLMwtr$DateTime)) > as.difftime(366,units="days")){
    stop("GLM ice time series must be equal or shorter than one year")
  }
  if (anyDep==TRUE){
    tempRef <-  getDailyTempMax(GLMwtr)
    tmpIdx = which(tempRef>temperature)
    
    #Check that we have at least one timepoint matching criteria
    if(length(tmpIdx) < 1){
      return(NA)
    }
    lastIdx <-  min(tmpIdx)
  }else{
    tempRef <-  getDailyTempMin(GLMwtr)
    tmpIdx = which(tempRef>temperature)
    
    #Check that we have at least one timepoint matching criteria
    if(length(tmpIdx) < 1){
      return(NA)
    }
    lastIdx <-  min(tmpIdx)
  }
  firstDOYabove  <-  GLMwtr[timeID][lastIdx,]
  return(firstDOYabove)
}

################################################################################
getStratifiedDuration <-  function(GLMwtr,GLMice,minStrat){
  if(diff(range(GLMwtr$DateTime)) > as.difftime(366,units="days")){
    stop("GLM ice time series must be equal or shorter than one year")
  }
  # advised that the input is shortened to the ice-free period,
  #Check to see if we found an ice on/off date. Sometimes, model was not
  #started early enough or ended late enough to have on/off date
  startDate <- as.character(getIceOffDate(GLMice,GLMwtr))
  if(is.na(startDate)){
    startDate = as.character(min(GLMwtr[,timeID]))
  }
  
  stopDate <- as.character(getIceOnDate(GLMice,GLMwtr))
  if(is.na(stopDate)){
    stopDate = as.character(max(GLMwtr[,timeID]))
  }
    
  GLMwtr <- subsetTime(GLMwtr,startDate,stopDate)
  tempMxMn <- cbind(getDailyTempMax(GLMwtr),getDailyTempMin(GLMwtr)) 
  stratDur  <-  sum(tempMxMn[,1]-tempMxMn[,2]>=minStrat)
  return(stratDur)
}

################################################################################

################################################################################
getStratifiedStartEnd <-  function(GLMwtr,GLMice,minStrat){
  if(diff(range(GLMwtr$DateTime)) > as.difftime(366,units="days")){
    stop("GLM ice time series must be equal or shorter than one year")
  }
  # advised that the input is shortened to the ice-free period,
  startDate <- as.character(getIceOffDate(GLMice,GLMwtr))
  stopDate <- as.character(getIceOnDate(GLMice,GLMwtr))
  GLMwtr <- subsetTime(GLMwtr,startDate,stopDate)
  tempMxMn <- cbind(getDailyTempMax(GLMwtr),getDailyTempMin(GLMwtr)) 
  startEndI <-  which(tempMxMn[,1]-tempMxMn[,2]>=minStrat)
  
  return(GLMwtr$DateTime[c(min(startEndI),max(startEndI))])
}

################################################################################
# This function conservatively estimates the mix start and end date
# Unlike finding the first and last possible date with minStrat delT, this
# finds the latest spring mixing (delT < minStrat) and latest fall
# mixing (delT < minStrat)
################################################################################
getUnmixedStartEnd <-  function(GLMwtr, GLMice, minStrat, arr.ind=FALSE){
  if(diff(range(GLMwtr$DateTime)) > as.difftime(366,units="days")){
    stop("GLM ice time series must be equal or shorter than one year")
  }
  
  # advised that the input is shortened to the ice-free period,
  startDate <- getIceOffDate(GLMice,GLMwtr)
  stopDate <- getIceOnDate(GLMice,GLMwtr)
  
  tempMxMn <- cbind(getDailyTempMax(GLMwtr),getDailyTempMin(GLMwtr))
  
  tempMxMn[GLMwtr$DateTime < startDate | GLMwtr$DateTime > stopDate , ] = NA
  
  #Ok, work out from the middle and find the closest day where
  # the lake did not meet the stratification criteria
  strat.end = floor(nrow(tempMxMn)/2)
  strat.start = floor(nrow(tempMxMn)/2)
  for(i in strat.end:nrow(tempMxMn)){
    if(is.na(tempMxMn[i,1]) | tempMxMn[i,1]-tempMxMn[i,2]<=minStrat){
      strat.end = max(strat.end, i - 1)
      break
    }
  }
  for(i in seq(strat.start, 1, by=-1)){
    if(is.na(tempMxMn[i,1]) | tempMxMn[i,1]-tempMxMn[i,2]<=minStrat){
      strat.start = min(strat.start, i + 1)
      break
    }
  }
  if(arr.ind){
    return(c(strat.start, strat.end))
  }else{
    return(GLMwtr$DateTime[c(strat.start, strat.end)])
  }
  
}

################################################################################
# volInTemp.GLM
#
# Calculates the total volume within a temperature range.
#
################################################################################
volInTemp.GLM <- function(GLMnc, lowT, highT, censor.days = 0){
  
  layVol = get_raw(GLMnc,"V")
  layTemp = get_raw(GLMnc,"temp")
  
  layVol = layVol[, censor.days:ncol(layVol)]
  layTemp = layTemp[, censor.days:ncol(layTemp)]
  
  volumes = vector(mode="double", length=ncol(layVol))*NaN
  
  
  for(i in 1:length(volumes)){
    volumes[i] = sum(layVol[layTemp[,i] >= lowT & layTemp[,i] <= highT ,i], na.rm=TRUE)
  }
  
  return(volumes)
}


################################################################################
# volInTemp.GLM
#
# Calculates the total height of the water column within a temperature range.
#
################################################################################
heightInRange.GLM <- function(GLMnc, lowT, highT, censor.days = 0){
  
  layZ = get_raw(GLMnc,"z")
  layTemp = get_raw(GLMnc,"temp")
  
  layZ = layZ[, censor.days:ncol(layZ)]
  layTemp = layTemp[, censor.days:ncol(layTemp)]
  
  thicks = vector(mode="double", length=ncol(layZ))*NaN
  #times = getTimeGLMnc(GLMnc)
  
  for(i in 1:length(thicks)){
    layer_dzs = diff(c(0, layZ[,i]))
    thicks[i] = sum(layer_dzs[layTemp[,i] >= lowT & layTemp[,i] <= highT ], na.rm=TRUE)
  }
  
  return(thicks)
}


################################################################################
# volInTemp.GLM
#
# Pulls volumes above a certain height.
#
################################################################################
volsAboveHeight.GLM <- function(GLMnc, heights){
  
  layZ = get_raw(GLMnc,"z")
  layZ[layZ > 1e10] = NA
  layVol = get_raw(GLMnc,"V")
  
  volumes = rep(NaN, length(heights))
  
  for(i in 1:length(volumes)){
    if(is.na(heights[i])){
      volumes[i] = NA
    }
    volumes[i] = sum(layVol[layZ[,i] > heights[i], ], na.rm=TRUE)
  }
  
  return(volumes)
}

################################################################################
# volInTemp.GLM
#
# Pulls volumes below a certain height.
#
################################################################################
volsBelowHeight.GLM <- function(GLMnc, heights){
  
  layZ = get_raw(GLMnc,"z")
  layZ[layZ > 1e10] = NA
  layVol = get_raw(GLMnc,"V")
  
  volumes = rep(NaN, length(heights))
  
  for(i in 1:length(volumes)){
    if(is.na(heights[i])){
      volumes[i] = NA
    }
    volumes[i] = sum(layVol[layZ[,i] < heights[i], ], na.rm=TRUE)
  }
  
  return(volumes)
}

#Returns water level from the NC file
#Should go in GLM.nc.R in rGLM at some point
water.level.glm = function(glm.nc){
  
  sim.z = get_raw(glm.nc,'z')
  sim.z[sim.z > 1e10] = NA
  
  z.out = apply(sim.z,2,max, na.rm=TRUE)
  
  return(z.out)
}







