## Check linear slopes of LTER summer surface and depth data data
library(plyr)
library(data.table)

wtr = read.table('../../../inst/supporting_files/wtemp.obs.tsv', sep='\t', header=TRUE)
wtr$DATETIME = as.POSIXct(wtr$DATETIME)
wtr$MONTH = as.POSIXlt(wtr$DATETIME)$mon + 1
wtr$YEAR  = as.POSIXlt(wtr$DATETIME)$year + 1900
lter.north = read.table('../../../inst/supporting_files/lter.north.lakes.tsv', sep='\t', header=TRUE, as.is=TRUE)


for(i in 1:nrow(lter.north)){
	cat(lter.north$Name[i], '\n')
	
	data = wtr[wtr$WBIC == lter.north$WBIC[i],]
	
	max.depth = max(data$DEPTH, na.rm=TRUE)
	
	shallow.i = data$DEPTH/max.depth < 0.3
	deep.i = data$DEPTH/max.depth > 0.7
	
	summer.i = data$MONTH == 8 | data$MONTH == 9 | data$MONTH == 7
	
	
	surf.means = ddply(data[shallow.i & summer.i,c("YEAR", "WTEMP")], c('YEAR'), function(df){mean(df$WTEMP)})
	
	mod = lm(V1~YEAR, surf.means)
	print(coefficients(mod))
	
	deep.means = ddply(data[deep.i & summer.i,c("YEAR", "WTEMP")], c('YEAR'), function(df){mean(df$WTEMP)})
	mod = lm(V1~YEAR, deep.means)
	print(coefficients(mod))
	

}

