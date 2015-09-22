
################################################################################
#
################################################################################
getDaysBetweenT.continuous <-  function(wtr, temperatureLow, temperatureHigh, anyDep=TRUE){
	# ANY or ALL depths, default is ANY
	temp <- wtr
	if (anyDep==TRUE){
		tempRangeCount  <-  sum(apply(temp,1,function(x) any(x>=temperatureLow,na.rm=TRUE & x<=temperatureHigh,na.rm=TRUE)))
	}else{
		tempRangeCount  <-  sum(apply(temp,1,function(x) all(x>=temperatureLow,na.rm=TRUE 
																	& x<=temperatureHigh,na.rm=TRUE)))
	}
	return(tempRangeCount)
}

################################################################################
# volInTemp.GLM
#
# Calculates the total volume within a temperature range.
#
################################################################################
volInTemp.GLM.continuous <- function(vol_raw, wtr_raw, lowT, highT, censor.days = 0){
	
	layVol = vol_raw
	layTemp = wtr_raw
	
	if(ncol(vol_raw) !=ncol(wtr_raw)){
		stop('Input volume and temps must be same length in time')
	}
	
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
heightInRange.GLM.continuous <- function(wtr_raw, z_raw, lowT, highT, censor.days = 0){
	
	layZ = z_raw
	layTemp = wtr_raw
	
	if(ncol(z_raw) !=ncol(wtr_raw)){
		stop('Input depths and temps must be same length in time (ncol)')
	}
	
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


#Returns water level from the NC file
#Should go in GLM.nc.R in rGLM at some point
water.level.glm.continuous = function(z_raw){
	
	sim.z = z_raw
	sim.z[sim.z > 1e10] = NA
	
	z.out = apply(sim.z,2,max, na.rm=TRUE)
	
	return(z.out)
}


################################################################################
# volInTemp.GLM
#
# Pulls volumes above a certain height.
#
################################################################################
volsAboveHeight.GLM.continuous <- function(raw_z, raw_vol, heights){
	
	layZ = raw_z
	layZ[layZ > 1e10] = NA
	layVol = raw_vol
	layVol[layVol > 1e10] = NA
	
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
volsBelowHeight.GLM.continuous <- function(raw_z, raw_vol, heights){
	
	layZ = raw_z
	layZ[layZ > 1e10] = NA
	layVol = raw_vol
	layVol[layVol > 1e10] = NA
	
	volumes = rep(NaN, length(heights))
	
	for(i in 1:length(volumes)){
		if(is.na(heights[i])){
			volumes[i] = NA
		}
		volumes[i] = sum(layVol[layZ[,i] < heights[i], ], na.rm=TRUE)
	}
	
	return(volumes)
}



