#Roundup these for kevin
# 10. Lake area*
# 11. Lake depth*
# 13. Kd
# 14. Latitude
# 17. Annual mean air temp
# 18. wind sheltering coefficient 
library(data.table)
library(plyr)
source('Libraries/GLM.functions.R')

driver.dir = 'D:/WiLMA/MetDrivers/07-30-appended2012'

tmp = fread('../supporting files/managed_lake_info.txt')


uWBIC=unique(tmp$WBIC)

all.output = data.frame()


for(i in 1:length(uWBIC)){
	wbic.char = as.character(uWBIC[i])
	
	driver.file = paste0(driver.dir, '/WBIC_', wbic.char, '.csv')
	if(!file.exists(driver.file)){
		next
	}
	
	output = data.frame(WBIC=uWBIC[i])
	
	
	drivers = fread(driver.file)
	drivers[,year:=as.POSIXlt(time)$year+1900]
	
	airt.means = ddply(drivers,'year', function(df){mean(df$AirTemp)})
	names(airt.means) = c('year','annual_airt')
	airt.means$WBIC = uWBIC[i]
	
	latlon            = getLatLon(wbic.char)
	output$lat        = latlon[1]
	output$depth_mx   = getZmax(wbic.char)
	output$depth_mean = getZmean(wbic.char)
	output$area       = getArea(wbic.char)
	output$Wstr       = getWstr(wbic.char)
	
	all.output = rbind(all.output, merge(airt.means, output, by='WBIC'))
	cat(i,'\n')
}

fOut = 'D:/lake_specific_metrics.tsv'
write.table(all.output, fOut, sep='\t', col.names=TRUE, row.names=FALSE)



