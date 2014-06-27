nldas.rods <- function(var.name,lat,lon){
  require(RCurl)
  num.heads <- 38
  data.names <- list('precipitation'='FORA0125_H.002:APCPsfc',
                     'sw_radiation'='FORA0125_H.002:DSWRFsfc',
                     'pt_evaporation'='FORA0125_H.002:PEVAPsfc',
                     'sp_humidity'='FORA0125_H.002:SPFH2m',
                     'air_temperature'='FORA0125_H.002:TMP2m',
                     'u_wind'='FORA0125_H.002:UGRD10m',
                     'v_wind'='FORA0125_H.002:UGRD10m')
                     
  base.url <- "http://hydro1.sci.gsfc.nasa.gov/daac-bin/access/timeseries.cgi"
  location <- paste('GEOM:POINT(',lon,',%20',lat,')',sep='')
  variable <- paste('NLDAS:NLDAS_',data.names[[var.name]],sep='')
  call.url <- paste(base.url,'?variable=',variable,'&location=',location,sep='')
  
  data.rod <- getURL(url=call.url) # big text block
  data.rod <- strsplit(data.rod,'\n') # into lines
  
  if (data.rod[[1]][num.heads]!="           Date&Time       Data"){
    warning("format for data.rods may have changed. proceed w/ caution")
  }
  l.file <- length(data.rod[[1]])
  data <- data.rod[[1]][(num.heads+1):(l.file-1)]
  data <- gsub(pattern = '      ',replacement = '',x = data)
  data <- gsub(pattern = '     ',replacement = '',x = data)
  data <- strsplit(data,'Z')
  data <- unlist(data)
  time <- as.POSIXct(x = strptime(data[seq(from = 1,to = length(data),by = 2)],"%Y-%m-%d %H"))
  obs <- as.numeric(data[seq(from = 2,to = length(data),by = 2)])
  nldas.rods <- data.frame("DateTime"=time,"value"=obs)
  return(nldas.rods)
}

downsample.rods <- function(nldas.rods,var.name){
  
  flat.dates <- strptime(nldas.rods$DateTime,'%Y-%m-%d')
  flat.vars <- nldas.rods$value
  dwn.dates <- unique(flat.dates)
  dwn.vars <- vector(mode='numeric',length = length(dwn.dates))
  
  for (j in 1:length(dwn.dates)){
    u.i <- flat.dates == dwn.dates[j]
    if (var.name=='precipitation'){
      dwn.vars[j] <- sum(flat.vars[u.i])
    } else {
      dwn.vars[j] <- sum(flat.vars[u.i])
    }
  }
  nldas.rods <- data.frame("DateTime"=as.POSIXct(dwn.dates),"value"=dwn.vars)
  return(nldas.rods)
}

write.rods <- function(nldas.rods,var.name,f.name){
  write.out <- nldas.rods
  names(write.out) <- c("DateTime",var.name)
  output = paste0('/Documents/R/Robertson/data/',f.name,'_',var.name,'.tsv')
  write.table(write.out,file=output,col.names=TRUE, quote=FALSE, row.names=FALSE, sep="\t")
}

var.name = "precipitation"
lakes <- list("Delevan"=c(42+36/60+0/3600,-88-36/60-30/3600),
              'Mendota'=c(42+6/60+0/3600,-89-25/60-0/3600),
              'Green'=c(42+36/60+0/3600,-88-36/60-30/3600),
              'Winnebago'=c(),
              'Anvil'=c(),
              'St_Germaine'=c())
lat = 42+36/60+0/3600
lon = -88-36/60-30/3600
nldas.rods <- nldas.rods(var.name,lat,lon)
nldas.rods <- downsample.rods(nldas.rods,var.name)
write.rods(nldas.rods,var.name,f.name='Delavan')
