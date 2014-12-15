
grl.resample.calcs = function(no.resamp=FALSE){
library(data.table)
library(plyr)
if(!exists('original.slopes')){
	tmp = fread('all.slopes.csv')
	tmp = tmp[,rel.depth:=floor(10*depth/zmax)/10]
	tmp = tmp[rel.depth <= 1,]
	tmp = tmp[,rel.depth5:=floor(5*depth/zmax)]
	#good.wbic = c(25300,28100,38700,45200,68300,97700,98600,131100,146100,163970,175700,186400,245650,299100,326400,345100,389300,396500,417400,462000,588000,608400,653700,672900,692400,692900,716800,741500,743000,747900,750300,750800,763600,772000,775000,777400,793600,804600,805400,809600,816800,827100,828000,830700,835800,848800,850800,852400,853200,854300,855200,858300,968100,968800,985100,1003700,1019500,1020400,1280400,1286500,1538600,1540400,1542400,1564200,1588200,1589100,1596600,1605800,1835300,1842400,1881100,1881900,2014700,2017500,2068000,2079700,2106800,2152800,2169200,2236800,2245100,2283300,2310200,2310400,2331600,2332400,2351400,2390800,2393200,2417000,2435700,2450500,2493100,2499000,2615100,2618000,2618100,2619400,2620000,2621100,2624200,2649500,2692900,2693800,2705400,2741600,2742700,2897100,2953500)
	
	year.range.lake = ddply(tmp,'wbic',function(df)max(df$end) - min(df$start))	
	good.wbic = year.range.lake[year.range.lake$V1 > 10, ]$wbic
	
	tmp = tmp[wbic%in%good.wbic, ]
	
	original.slopes <<- tmp
}

all.slopes = original.slopes

#num.per.lake = ddply(all.slopes,'wbic',function(df)nrow(df))

output = ddply(all.slopes, 'wbic', function(df){

	if(nrow(df) < 800){
		return(df[0,])
	}
	if(no.resamp){
		return(df)
	}else{
		return(df[sample(1:nrow(df), 800),])  #randomly subsample 800
	}
	
})

#write.csv(output,'downsampled.slopes.csv', row.names=FALSE)
return(output)
#lake.year.range = ddply(all.slopes, 'wbic', function(df)max(df$dt))
}
