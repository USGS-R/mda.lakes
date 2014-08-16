#From buildIceOffFiles.m

library(oce)
library(stringr)
library(lubridate)
library(zoo)
library(plyr)
source("Libraries/GLM.functions.R")

################################################################################
## Load validation ice on/off data
################################################################################
ice_data = read.table('../supporting files/Validation/ice_data.csv', header=TRUE, sep=',', as.is=TRUE)

ice_on = ice_data[,c('WBIC','iceon_year', 'iceon_month', 'iceon_day')]
ice_on = ice_on[complete.cases(ice_on), ]
ice_on$day_on = as.POSIXlt(ISOdate(ice_on$iceon_year,ice_on$iceon_month, ice_on$iceon_day))$yday

# now, we flip the iceon days that are in the next year
# and it is important to also flip the year back one. A january ice-on still belongs
# to the previous year's season
to_flip = ice_on$day_on < 70 & ice_on$day_on > 0
ice_on[to_flip,]$day_on = ice_on[to_flip,]$day_on + 365
ice_on[to_flip,]$iceon_year = ice_on[to_flip,]$iceon_year - 1

names(ice_on) = c('WBIC', 'year', 'month', 'day','yday_on')

## Ice off
ice_off = ice_data[,c('WBIC','iceoff_year', 'iceoff_month', 'iceoff_day')]

ice_off = ice_off[complete.cases(ice_off), ]
ice_off$day_off = as.POSIXlt(ISOdate(ice_off$iceoff_year,ice_off$iceoff_month, ice_off$iceoff_day))$yday

names(ice_off) = c('WBIC', 'year', 'month', 'day','yday_off')

################################################################################
## Generate all driver data for all WBICS
################################################################################

driver_root = 'D:/WiLMA/MetDrivers/07-30-appended2012'

all_inputs = Sys.glob(file.path(driver_root, '*.csv'))
wbics = unlist(str_extract_all(all_inputs, perl('(?<=WBIC_)[0-9]+')))

all_ice_inputs = data.frame()

for(i in 1:length(all_inputs)){
	
	cat('starting ', wbics[i], '\n')
	data = read.table(all_inputs[i], sep=',', header=TRUE, as.is=TRUE)
	data$time = as.POSIXct(data$time, tz='Etc/GMT+6')
	data$year = year(data$time)
	data$month = month(data$time)
	
	airt_mean = data.frame(mean30=rollmean(data$AirTemp, 30))
	airt_mean$time = data[1:(nrow(data)-29),]$time
	airt_mean$year = year(airt_mean$time)
	
	
	uyears = unique(year(data$time))
	lake_drivers = data.frame(year=uyears, zero_fl=NA, zero_sp=NA, mn_dp=NA, 
														at_fl=NA, ang_sp=NA, sa=NA, long=NA, el=NA)
	
	latlon = getLatLon(wbics[i])
	
	for(j in 1:length(uyears)){
		
		year_data = data[data$year >= uyears[j] & data$year <= (uyears[j]+1), ]
		year_airt_mean = airt_mean[airt_mean$year == uyears[j], ]
		
		n_airt_mean = nrow(year_airt_mean)
		
		#if we use the diff of the sign, we get a more discerning selection function
		zero_sp_ind = min(which(diff(sign(year_airt_mean[1:182,]$mean30))==2))+1
		zero_sp_datetime = with_tz(year_airt_mean$time[zero_sp_ind] + minutes(12*60), tzone = 'UTC')
		tmp = sunAngle(zero_sp_datetime, latitude = latlon[1], longitude=latlon[2])
		ang_sp = tmp$altitude
		
		
		zero_fl_ind = 182+min(which(diff(sign(year_airt_mean[183:n_airt_mean,]$mean30))==-2))
		
		if(!is.infinite(zero_fl_ind)){
			zero_fl_datetime = with_tz(year_airt_mean$time[zero_fl_ind] + minutes(12*60), tzone = 'UTC')
			tmp = sunAngle(zero_fl_datetime, latitude = latlon[1], longitude=latlon[2])
			ang_fl = tmp$altitude
	
			at_mon_start = month(zero_fl_datetime)-1
			at_year_start = year(zero_fl_datetime)
			if(at_mon_start <=0){
				at_mon_start = at_mon_start + 12
				at_year_start = at_year_start - 1
				
			}
			at_start = ISOdatetime(at_year_start, at_mon_start, 1, 0,0,0)
			
			at_mon_end = month(zero_fl_datetime)+2
			at_year_end = year(zero_fl_datetime)
			if(at_mon_end > 12){
				at_mon_end = at_mon_end - 12
				at_year_end = at_year_end + 1
			}
			at_end = ISOdatetime(at_year_end, at_mon_end, 1, 0, 0, 0)-24*60*60
			
			at_interval = new_interval(at_start, at_end)
			
			at_fl = mean(data[data$time %within% at_interval, ]$AirTemp)
			
		}else{
			ang_fl = NA
			zero_fl_ind = NA
			ang_fl = NA
			at_fl = NA
		}
		
		lake_drivers$zero_fl[j] = zero_fl_ind
		lake_drivers$zero_sp[j] = zero_sp_ind
		lake_drivers$ang_fl[j]  = ang_fl
		lake_drivers$ang_sp[j]  = ang_sp
		lake_drivers$at_fl[j]   = at_fl
		
	}
	
	#Get all lake-specific, year-agnostic data
	lake_drivers$mn_dp = getZmean(wbics[i])
	lake_drivers$sa    = getArea(wbics[i])*1e-6
	lake_drivers$long  = latlon[2]
	lake_drivers$el    = getElevation(wbics[i])
	lake_drivers$WBIC  = wbics[i]
	
	#join to main data.frame
	all_ice_inputs = rbind(all_ice_inputs, lake_drivers)
	
}


###Cleanup output
#drop lakes with missing sa or long, or el or mn_dp
all_ice_inputs = all_ice_inputs[complete.cases(all_ice_inputs[,c('mn_dp','sa','long','el')]), ]



################################################################################
## Build ice models
################################################################################

## Ice off
ice_off_mod = merge(all_ice_inputs, ice_off, by=c('WBIC', 'year'))
lm_off = lm(yday_off~zero_sp+ang_sp+sa+long+el, ice_off_mod)

## Ice on
ice_on_mod = merge(all_ice_inputs, ice_on, by=c('WBIC', 'year'))
ice_on_mod$sqrt_mn_dp = ice_on_mod$mn_dp^0.5

lm_on  = lm(yday_on~zero_fl+sqrt_mn_dp+at_fl, ice_on_mod)


all_ice_inputs$yday_off_predict = floor(predict(lm_off, all_ice_inputs))
all_ice_inputs$sqrt_mn_dp = all_ice_inputs$mn_dp^0.5
all_ice_inputs$yday_on_predict  = floor(predict(lm_on, all_ice_inputs))

all_ice_inputs$yday_off_predict[is.na(all_ice_inputs$yday_off_predict)] = 
	median(all_ice_inputs$yday_off_predict, na.rm=TRUE)

all_ice_inputs$yday_on_predict[is.na(all_ice_inputs$yday_on_predict)] = 
	median(all_ice_inputs$yday_on_predict, na.rm=TRUE)


on.output = all_ice_inputs[, c('WBIC','yday_on_predict', 'year')]
off.output = all_ice_inputs[, c('WBIC','yday_off_predict', 'year')]

on.output$ON.OFF = 'on'
on.output$DATE = format(ISOdate(on.output$year, 1, 1) + (on.output$yday_on_predict-1) *24*60*60, '%F')
names(on.output) = c('WBIC','doy','ice.year','ON.OFF','DATE')

off.output$ON.OFF = 'off'
off.output$DATE = format(ISOdate(off.output$year, 1, 1) + (off.output$yday_off_predict-1) *24*60*60, '%F')
names(off.output) = c('WBIC','doy','ice.year','ON.OFF','DATE')


write.table(rbind(off.output, on.output), '../supporting files/empirical.ice.tsv',
						quote=FALSE, sep='\t', row.names=FALSE, col.names=TRUE)



