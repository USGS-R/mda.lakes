#Build sampling table for grl


library(data.table)
library(plyr)
source('grl.resample.calcs.R')

metrics.calc = function(df){
	
	out = data.frame(id=1)
	
	out$nevents = length(unique(df$DATETIME))
	out$avgobs = round(out$nevents/length(unique(df$YEAR)))
	out$first = min(df$YEAR)
	out$last = max(df$YEAR)
	out$missingyear= 23-length(unique(df$YEAR))
	out$nobs = nrow(df)
	
	return(out[,-1])
}

wtr = fread('../../../inst/supporting_files/wtemp.obs.tsv')
#from previous subset method
# wbics = c(25300,28100,30300,38700,45200,68300,97700,
# 					98600,104000,117500,131100,146100,162500,
# 					163970,179800,180300,186400,195400,287200,
# 					299100,322800,345100,373500,389300,394400,
# 					396500,406900,417400,555700,588000,653700,
# 					672900,692400,692900,716800,737900,741500,
# 					747900,750800,763600,766600,775000,779300,
# 					793600,804600,805000,805400,826300,827100,
# 					830700,835800,843400,850800,853200,854300,
# 					855200,968100,968800,969600,981200,985100,
# 					1019500,1022900,1260600,1296000,1345700,1377100,
# 					1378100,1412200,1527800,1538600,1540400,1542400,1564200,1569900,1579900,
# 					1588200,1589100,1593800,1596300,1601300,1605800,1623800,1629500,
# 					1630100,1835300,1835500,1842400,1881900,2014700,2017500,2068000,
# 					2079700,2079800,2081200,2094100,2106800,2109600,2152800,2236800,
# 					2283300,2294900,2306300,2310200,2310400,2320800,2330800,2331600,
# 					2332400,2351400,2390800,2392000,2393200,2417000,2435000,2435700,
# 					2453500,2460700,2485700,2490500,2493100,2495100,2499000,2615100,
# 					2615900,2616100,2618000,2618100,2619400,2620000,2624200,2641000,
# 					2650900,2661100,2678100,2692900,2693700,2693800,2694000,2858100,
# 					2897100,2953500)

wbics = unique(grl.resample.calcs(no.resamp=TRUE)$wbic)

#wtr = wtr[WBIC%in%wbics, ]

wtr[,DATETIME:= as.POSIXct(DATETIME)]
wtr[,YEAR:=as.POSIXlt(DATETIME)$year+1900]
wtr = wtr[YEAR >= 1990,]

metrics = ddply(wtr, 'WBIC', metrics.calc)
metrics$missingyear[metrics$missingyear < 0] = 0

metrics$included = 'No'
metrics$included[metrics$WBIC%in%wbics] = 'Yes'
metrics = metrics[order(metrics$nevents, decreasing = TRUE),]
metrics = metrics[order(metrics$included, decreasing = TRUE),]


names(metrics) = c('WBIC', '# Sampling Events', '# Samples per Year', 'Earliest Year', 'Latest Year', 'N Years Unsampled (from 1990-2012)', '# Individual Observations', 'In Analysis')


write.table(metrics, 'lake.sampling.metrics.csv', sep=',', 
						row.names=FALSE)



