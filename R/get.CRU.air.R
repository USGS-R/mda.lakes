
getCRU	<-	function(lat,long, use.years = seq(1985,2009)){
	require(ncdf4)

	
  
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


require(ncdf4)
ME <- getCRU(lat=43,long=-89, use.years = seq(1979,2011))