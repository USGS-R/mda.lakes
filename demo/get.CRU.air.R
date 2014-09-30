
getCRU	<-	function(WBIC, use.years = seq(1985,2009)){
  lat.lon <- getLatLon(WBIC)
  require(ncdf4)

	lat = lat.lon[1]
  long = lat.lon[2]
  
  vals = get.vals(lat,long,years='1981.1990')

  CA = vals
  
	vals = get.vals(lat,long,years='1991.2000')
	CA	<-	rbind(CA,vals)
  
	vals = get.vals(lat,long,years='2001.2010')
	CA	<-	rbind(CA,vals)
  
  CRU = CA
	return(CRU)
}

get.vals <- function(lat,long,years){
  
  start.time = 1
  
  data.dir  <-	"/Users/jread/Documents/GLTC-stats/rGLTC/data/CRU_ts3.21/"
  nc	<-	nc_open(filename=paste(data.dir,'cru_ts3.21.',years,'.tmp.dat.nc',sep=''))
  lat.vals	<-	ncvar_get(nc,varid="lat")
  lon.vals	<-	ncvar_get(nc,varid="lon")
  days.since <- ncvar_get(nc,varid="time")
  
  lat.i	<-	which.min(abs(lat.vals-lat))[1]
  lon.i	<-	which.min(abs(lon.vals-long))[1]
  vals = ncvar_get(nc=nc, varid="tmp",start=c(lon.i,lat.i,start.time),count=c(1,1,length(days.since)))
  nc_close(nc)
  
  days.since = as.Date('1900-01-01')+days.since
  df <- data.frame("DateTime"=days.since,"tmp"=vals)
  return(df)
}

getNLDAS <- function(WBIC,time.in){
  root.dir <- '/Users/jread/Documents/WiLMA/supporting files/all_driver/'
  data <- read.csv(paste0(root.dir,'WBIC_',WBIC,'.csv'))
  
  time <- as.Date(data$time)
  air <- as.numeric(data$AirTemp)
  
  time.out <- time.in
  
  dat.out <- vector(length=length(time.in))
  
  for (j in 1:length(time.in)){
    # assumes CRU format
    d.1 <- as.Date(paste0(format(time.in[j], "%Y-%m"),'-1'))
    d.2 <- as.Date(paste0(format(time.in[j]+30, "%Y-%m"),'-1'))
    u.i <- time < d.2 & time >= d.1
    dat.out[j] <- mean(air[u.i])
  }
  
  return(data.frame('time'=time.out,"air"=dat.out))
}

require(ncdf4)
source('Libraries/GLM.functions.R')

get.air.coef <- function(nldas,cru){
  
  y <- cru[, 2]
  x <- nldas[, 2]
  lm <- lm(y ~ x)
  b = lm$coef[[1]]
  m = lm$coef[[2]]
  coefs <- c(m,b)
  return(coefs)
}

build.lake.coef <- function(){
  require(stringr)
  model.dirs = Sys.glob('/Users/jread/Documents/WiLMA/supporting files/all_driver/WBIC_*')
  model.ids = basename(model.dirs)
  WBICs = str_extract(model.ids,'\\d+')  # WBICS as strings
  num.lks <- length(WBICs)
  
  na.vec <- vector(length=num.lks)
  
  nldas.coef <- data.frame('WBIC'=WBICs,"m"=na.vec,'b'=na.vec)
  for (i in 1:num.lks){
    WBIC = WBICs[i]
    cru <- getCRU(WBIC, use.years = seq(1979,2011))
    nldas <- getNLDAS(WBIC,time.in=cru$DateTime)
    coef <- get.air.coef(nldas,cru)
    nldas.coef$m[i] = coef[1]
    nldas.coef$b[i] = coef[2]
    print(paste0('done with WBIC: ',WBIC))
  }
  return(nldas.coef)
}

write.nldas.coef <- function(){
  nldas.coef <- build.lake.coef()
  write.table(x=nldas.coef,file='../supporting files/nldas_coef.tsv',
              quote=F,row.names=T,col.names=T,sep='\t')
}

write.nldas.coef()

