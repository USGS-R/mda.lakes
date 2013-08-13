## Code to calculate important derivatives for chained runs

chained.habitat.calc = function(run.path, output.path, lakeid){
  
  require(stringr)
  require(rGLM)
  
  nc.files = Sys.glob(file.path(run.path, '*.nc'))
  years = wbics = str_extract(basename(nc.files),"[0-9]+")
  
  if(missing(lakeid)){
    lakeid = basename(run.path)
  }
  
  vol.tmp.ranges = c(28,29,     10.6,11.2,
                     12,28,     18.2,28.2, 
                     19.3,23.3, 19, 23, 
                     20.6,23.2, 22, 23,
                     25, 29,    26.6, 32,
                     26, 28,    26, 32,
                     28,32,     29, 100,
                     30, 31,    18, 22)
  
  vol.tmp.ranges = matrix(vol.tmp.ranges, ncol=2, byrow=TRUE)
  
  #These are basically the same ranges
  height.tmp.ranges = vol.tmp.ranges
  
  day.tmp.ranges = c(19, 23,       26, 30,
                     10.6, 11.2,   12, 28,
                     18.2, 28.2,   18, 22,
                     20.6, 23.2,   20, 30,
                     22, 23,       23, 31,
                     26.6, 32,     26, 28,
                     28, 29,       30, 31,
                     19.3, 23.3,   29, 100,
                     21, 100)
  day.tmp.ranges = matrix(day.tmp.ranges, ncol=2, byrow=TRUE)
  
  
  volumes.out = list()
  heights.out = list()
  days.out = list()
  
  misc.out = list()
  
  bad = rep(FALSE, length(years))
  
  for(i in 1:length(nc.files)){
    
    #Open the NC file and get data frame of wtr temp
    GLMnc = nc_open(nc.files[i], readunlim=FALSE)
    test.get = ncvar_get(GLMnc,'temp')
    
    #if(length(dim(test.get)) < 2){
      #bad[i] = TRUE
      
      #next
    #}
      
    wtr = getGLMwtr(GLMnc)
    ice = getGLMice(GLMnc)
    surfT = getSurfaceT(wtr)
    
    #Iterate through all ranges and store in name-indexed list
    
    for(j in 1:nrow(vol.tmp.ranges)){
      #Name of volume temp range
      vol.name = paste('vol', vol.tmp.ranges[j,1], vol.tmp.ranges[j,2], sep='_')
      
      #Calc 
      tmp = volInTemp.GLM(GLMnc, vol.tmp.ranges[j,1], vol.tmp.ranges[j,2])
      
      #add to vector
      volumes.out[[vol.name]] = c(volumes.out[[vol.name]], sum(tmp[[2]])*1000)
    }
    
    #Now do vertical length of water column in temperature range.
    for(j in 1:nrow(height.tmp.ranges)){
      height.name = paste('height', height.tmp.ranges[j,1], height.tmp.ranges[j,2], sep='_')
      tmp = heightInRange.GLM(GLMnc, height.tmp.ranges[j,1], height.tmp.ranges[j,2])
      
      heights.out[[height.name]] = c(heights.out[[height.name]], mean(tmp, na.rm=TRUE))
    }
    
    for(j in 1:nrow(day.tmp.ranges)){
      name = paste('days', day.tmp.ranges[j,1], day.tmp.ranges[j,2], sep='_')
      days.out[[name]] = c(days.out[[name]], getDaysBetweenT(wtr, day.tmp.ranges[j,1], day.tmp.ranges[j,2]))
    }
    
    
    misc.out[['peak_temp']] = c(misc.out[['peak_temp']], getTempMax(wtr))
    misc.out[['durStrat']] = c(misc.out[['durStrat']], getStratifiedDuration(wtr, ice, minStrat=0.5))
    
    
    misc.out[['dateOver8.9']] = c(misc.out[['dateOver8.9']], getFirstDayAboveT(wtr, 8.9))
    misc.out[['dateOver21']] = c(misc.out[['dateOver21']], getFirstDayAboveT(wtr, 21))
    misc.out[['dateOver18']] = c(misc.out[['dateOver18']], getFirstDayAboveT(wtr, 18))
    misc.out[['dateOver16.7']] = c(misc.out[['dateOver16.7']], getFirstDayAboveT(wtr, 16.7))
    
    misc.out[['coef_var_0_30']] = c(misc.out[['coef_var_0_30']], sd(surfT[1:30])/mean(surfT[1:30]))
    misc.out[['coef_var_30_60']] = c(misc.out[['coef_var_30_60']], sd(surfT[31:60])/mean(surfT[31:60]))
    
    tmpDay = 1:30
    misc.out[['post_ice_warm_rate']] = c(misc.out[['post_ice_warm_rate']], lm(surfT[1:30]~tmpDay)$coefficients[2])
    
    jun1 = as.POSIXct(paste(years[i], '-06-01', sep=''))
    jul1 = as.POSIXct(paste(years[i], '-07-01', sep=''))
    jul31 = as.POSIXct(paste(years[i], '-07-31', sep=''))
    sep30 = as.POSIXct(paste(years[i], '-09-30', sep=''))
    
    misc.out[['mean_surf_jul']] = c(misc.out[['mean_surf_jul']],
                                    mean(getSurfaceT(wtr[wtr$DateTime >= jul1 & wtr$DateTime <= jul31, ])))
    misc.out[['mean_surf_JAS']] = c(misc.out[['mean_surf_JAS']],
                                    mean(getSurfaceT(wtr[wtr$DateTime >= jul1 & wtr$DateTime <= sep30, ])))
    
    
    misc.out[['spring_days_in_10.5_15.5']] = c(misc.out[['spring_days_in_10.5_15.5']],
                                               getDaysBetweenT(wtr[wtr$DateTime < jun1, ], 10.5, 15.5))
    
    elevations = getElevGLM(wtr)
    depths = max(elevations) - elevations
    tmp = getEpiMetaHypo.GLM(wtr, depths)
    start.end = getUnmixedStartEnd(wtr, ice, 0.5, arr.ind=TRUE)
    
    misc.out[['SthermoD_mean']] = c(misc.out[['SthermoD_mean']], mean(tmp$SthermoD[start.end[1]:start.end[2]], na.rm=TRUE))
    misc.out[['metaTopD_mean']] = c(misc.out[['metaTopD_mean']], mean(tmp$metaTopD[start.end[1]:start.end[2]], na.rm=TRUE))
    
    
    #Cleanup this memory hog
    nc_close(GLMnc)
    cat("Vols calculated", years[i], '\n')
  }
  
  #Build output data frame
  fOutput = data.frame(year=years[!bad])
  fOutput$lakeid = lakeid
  
  vol.names = names(volumes.out)
  for(i in 1:length(vol.names)){
    fOutput[[vol.names[i]]] = volumes.out[[vol.names[i]]]
  }
  
  height.names = names(heights.out)
  for(i in 1:length(height.names)){
    fOutput[[height.names[i]]] = heights.out[[height.names[i]]]
  }
  days.names = names(days.out)
  for(i in 1:length(days.names)){
    fOutput[[days.names[i]]] = days.out[[days.names[i]]]
  }
  misc.names = names(misc.out)
  for(i in 1:length(misc.names)){
    
    fOutput[[misc.names[i]]] = misc.out[[misc.names[i]]]
  }
  
  #Output!!
  write.table(fOutput, output.path, row.names=FALSE, sep='\t')
}




