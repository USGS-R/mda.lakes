
library(glmtools)
library(mda.lakes)

Sys.setenv(TZ='GMT')

ice_data = read.table(system.file('supporting_files/Validation/ice_data.csv', package='mda.lakes'), header=TRUE, sep=',', as.is=TRUE)

uids = as.character(unique(ice_data$WBIC))
glm_ice = data.frame()

for(i in 1:length(uids)){
	
	dest = 'D:/test'
	
	
	if(is.na(getClarity(uids[i], default.if.null=FALSE)) | is.null(getZmax(uids[i]))){
		next
	}
	
	prep_run_glm_kd(uids[i], path=dest, start='1979-04-01', end='2011-01-01', nml_args=list(dt=3600, subdaily=FALSE, nsave=24))
	
	ice = get_ice(file.path(dest, 'output.nc'))
	wtr = get_temp(file.path(dest, 'output.nc'), reference = 'surface')
	
	ice_onoff = get_ice_onoff(ice, wtr)
	ice_onoff$WBIC = uids[i]
	
	glm_ice = rbind(glm_ice, ice_onoff)
}

names(glm_ice) = c('ice.year', 'on', 'off', 'wbic')
tmp_on = glm_ice[, c('ice.year', 'on', 'wbic')]
tmp_on$on.off = 'on'
tmp_on = tmp_on[complete.cases(tmp_on),]
tmp_on$on = as.POSIXlt(tmp_on$on)$yday+1
to_flip = tmp_on$on < 70 & tmp_on$on > 0
tmp_on[to_flip,]$on = tmp_on[to_flip,]$on + 365


tmp_off = glm_ice[, c('ice.year', 'off', 'wbic')]
tmp_off$on.off = 'off'
tmp_off$off = as.POSIXlt(tmp_off$off)$yday+1

names(tmp_on) = c('ice.year', 'glm_doy', 'wbic', 'on.off')
names(tmp_off) = c('ice.year', 'glm_doy', 'wbic', 'on.off')

glm_ice = rbind(tmp_on, tmp_off)


##########################################################################################
## Compare cal data with GLM modeled data
##########################################################################################

ice_obs = read.table(system.file('supporting_files/ice.obs.tsv', package='mda.lakes'), sep='\t', header=TRUE)
names(ice_obs) = tolower(names(ice_obs))
ice_obs = ice_obs[,c('wbic', 'ice.year', 'doy', 'on.off')]
ice_obs$doy[ice_obs$doy < 0] = ice_obs$doy[ice_obs$doy < 0] + 365


both_ice = merge(ice_obs, glm_ice, by=c('wbic', 'ice.year', 'on.off'))
both_ice$emp_doy = NA

uyear = unique(both_ice$ice.year)

for(i in 1:length(uyear)){
	
	indx = both_ice$ice.year == uyear[i] & both_ice$on.off == 'on'
	both_ice$emp_doy[indx] = getIceOn(both_ice$wbic[indx], uyear[i])
	
	indx = both_ice$ice.year == uyear[i] & both_ice$on.off == 'off'
	both_ice$emp_doy[indx] = getIceOff(both_ice$wbic[indx], uyear[i])
	cat(i,'\n')
}

both_ice$emp_doy = as.POSIXlt(both_ice$emp_doy)$yday+1
flip_indx = both_ice$emp_doy < 70 & both_ice$on.off == 'on'
both_ice$emp_doy[flip_indx] = both_ice$emp_doy[flip_indx] + 365

plot(both_ice$doy, both_ice$emp_doy, ylab='Modeled', xlab='Observed')
points(both_ice$doy, both_ice$glm_doy, col='red')
abline(0,1)
legend('topleft', legend=c('Shuter et al', 'GLM'), pch=1, col=c('black', 'red'))

tmp = both_ice[both_ice$on.off == 'on', ]
plot(tmp$doy, tmp$emp_doy, ylab='Modeled', xlab='Observed')
points(tmp$doy, tmp$glm_doy, col='red')
abline(0,1)
title('Ice on')
legend('topleft', legend=c('Shuter et al', 'GLM'), pch=1, col=c('black', 'red'))

tmp = both_ice[both_ice$on.off == 'off', ]
plot(tmp$doy, tmp$emp_doy, ylab='Modeled', xlab='Observed')
points(tmp$doy, tmp$glm_doy, col='red')
abline(0,1)
title('Ice on')
legend('topleft', legend=c('Shuter et al', 'GLM'), pch=1, col=c('black', 'red'))


##We are shooting for an ~8.5 day mean absolute error as reported in Shuter et al 2013
cat('MAE of GLM Overall, on and off date/time')
cat('Mean Absolute Errors\n')
mean(abs(both_ice$glm_doy - both_ice$doy))
mean(abs(both_ice[both_ice$on.off=='on',]$glm_doy - both_ice[both_ice$on.off=='on',]$doy))
mean(abs(both_ice[both_ice$on.off=='off',]$glm_doy - both_ice[both_ice$on.off=='off',]$doy))

cat('STD of GLM Overall, on and off date/time')
cat('Standard Deviation\n')
sd((both_ice$glm_doy - both_ice$doy))
sd((both_ice[both_ice$on.off=='on',]$glm_doy - both_ice[both_ice$on.off=='on',]$doy))
sd((both_ice[both_ice$on.off=='off',]$glm_doy - both_ice[both_ice$on.off=='off',]$doy))

cat('Bias of GLM Overall, on and off date/time')
cat('Bias\n')
mean((both_ice$glm_doy - both_ice$doy))
mean((both_ice[both_ice$on.off=='on',]$glm_doy - both_ice[both_ice$on.off=='on',]$doy))
mean((both_ice[both_ice$on.off=='off',]$glm_doy - both_ice[both_ice$on.off=='off',]$doy))

cat('MAE of empirical model overall, on and off date/time')
cat('Mean Absolute Errors\n')
mean(abs(both_ice$emp_doy - both_ice$doy))
mean(abs(both_ice[both_ice$on.off=='on',]$emp_doy - both_ice[both_ice$on.off=='on',]$doy))
mean(abs(both_ice[both_ice$on.off=='off',]$emp_doy - both_ice[both_ice$on.off=='off',]$doy))


cat('R^2')
tmp = both_ice[both_ice$on.off == 'off', ]
summary(lm(tmp$emp_doy ~ tmp$glm_doy))$r.squared
summary(lm(tmp$doy ~ tmp$glm_doy))$r.squared
summary(lm(tmp$doy ~ tmp$emp_doy))$r.squared


cat('RMSE of GLM model overall, on and off date/time')
cat('RMSE\n')
mean(sqrt(mean((both_ice$emp_doy - both_ice$doy)^2)))
mean(sqrt(mean((both_ice[both_ice$on.off=='on',]$emp_doy - both_ice[both_ice$on.off=='on',]$doy)^2)))
mean(sqrt(mean((both_ice[both_ice$on.off=='off',]$emp_doy - both_ice[both_ice$on.off=='off',]$doy)^2)))



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


