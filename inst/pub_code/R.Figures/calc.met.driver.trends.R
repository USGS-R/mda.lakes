#### Calc JAS trends for other climate inputs
library(plyr)
library(stringr)
Sys.setenv(TZ='GMT')

driver.files = Sys.glob('07-30/*.csv')
wbic.str = str_extract(basename(driver.files), '\\d+')
wbic.num = as.numeric(wbic.str)

all.means = data.frame()

for (i in 1:length(driver.files)){
  tmp = read.csv(driver.files[i], header=TRUE)
  
  months = as.POSIXlt(tmp$time)$mon+1
  years = as.POSIXlt(tmp$time)
  JAS.indx = months >= 6 & months <= 8
  
  #pull out only JAS
  tmp = tmp[JAS.indx, ]
  
  tmp$year = as.POSIXlt(tmp$time)$year+1900
  year.means = ddply(tmp[,-1], c('year'), function(df) data.frame(lapply(df, mean)))
  year.means$WBIC = wbic.num[i]
  all.means = rbind(all.means, year.means)
  cat(i, '\n')
}

write.table(all.means, 'driver.JAS.means.tsv', sep='\t', row.names=FALSE, col.names=TRUE)
