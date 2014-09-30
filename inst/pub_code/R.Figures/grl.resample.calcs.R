
library(data.table)
library(plyr)
if(!exists('original.slopes')){
	original.slopes = fread('all.slopes.csv')
}
all.slopes = original.slopes
all.slopes = all.slopes[,rel.depth:=floor(10*depth/zmax)/10]
all.slopes = all.slopes[rel.depth <= 1,]
all.slopes = all.slopes[,rel.depth5:=floor(5*depth/zmax)]


num.per.lake = ddply(all.slopes,'wbic',function(df)nrow(df))
year.range.lake = ddply(all.slopes,'wbic',function(df)max(df$end) - min(df$start))


good.wbic = year.range.lake[year.range.lake$V1 > 10, ]$wbic

all.slopes = all.slopes[wbic%in%good.wbic, ]


##Randomly subsample 1000? from each lake
output = data.frame()
for(i in 1:length(good.wbic)){
	
	sub.samp = all.slopes[wbic==good.wbic[i]]
	if(nrow(sub.samp) < 800){
		next
	}
	
	output = rbind(output, sub.samp[sample(1:nrow(sub.samp), 800),])  #randomly subsample 500
	
	#for(d in 0:4){
	#	tmp = sub.samp[rel.depth5==d,]
	#	output = rbind(output, tmp[sample(1:nrow(tmp),80),])  #randomly subsample 500
	#}
}



write.csv(output,'downsampled.slopes.csv', row.names=FALSE)

#lake.year.range = ddply(all.slopes, 'wbic', function(df)max(df$dt))

