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