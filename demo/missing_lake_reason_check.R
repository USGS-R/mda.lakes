library(mda.lakes)


tmp = read.table('~/../Desktop/missing_walleye_lakes.csv', sep=',', header=TRUE)
tmp$WBIC = as.character(tmp$WBIC)

tmp$hasbathy = TRUE
tmp$hasmet = TRUE
for(i in 1:nrow(tmp)){
  
  met = NULL
  
  tryCatch({met = get_driver_path(paste0('WBIC_', tmp$WBIC[i], '.csv'), loc_cache=FALSE)}, error=function(e){})
  if(is.null(met)){
    tmp$hasmet[i]=FALSE
  }
  
  bathy = NULL
  tryCatch({bathy = getBathy(tmp$WBIC[i])}, error=function(e){})
  if(is.null(bathy)){
    tmp$hasbathy[i]=FALSE
  }
  
  if(is.null(getZmax(tmp$WBIC[i]))){
    tmp$hasdepth[i]=FALSE
  }else{
    tmp$hasdepth[i]=TRUE
  }
  
}
