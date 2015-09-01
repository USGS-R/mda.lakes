# Sens slope of met drivers
library(mda.lakes)
library(plyr)
library(rLakeAnalyzer)
library(reshape2)
library(lubridate)

driver = read.csv(get_driver_path('WBIC_1881900.csv', driver_name = 'GENMOM'), header=TRUE)
driver$time = as.POSIXct(driver$time)

vars = names(driver)[-1]

for(i in 1:length(vars)){
  
  driver_slopes = sens_seasonal_site(year(driver$time), data = driver[,vars[i]], season_i = week(driver$time))
  
  season_slopes = ddply(driver_slopes, 'season_i', function(df){data.frame(med=median(df$slopes))})
  
  png(paste0('~/season.week.sens.', vars[i],'.png'), res=300, width=2000, height=1500)
  plot(season_slopes, type='l')
  abline(0,0)
  #boxplot(slopes~season_i, driver_slopes, main=vars[i])
  dev.off()

}
