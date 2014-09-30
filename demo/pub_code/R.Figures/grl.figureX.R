
library(data.table)
library(plyr)
all.slopes = fread('all.slopes.csv')
all.slopes = all.slopes[,rel.depth:=floor(10*depth/zmax)/10]
all.slopes = all.slopes[rel.depth <= 1,]


u.wbic = unique(all.slopes[order(area)]$wbic)
j = 1
output = matrix(nrow=11, ncol=length(u.wbic))
n.pairs = rep(NA, length(u.wbic))

for(i in 1:length(u.wbic)){
	tmp = all.slopes[wbic==u.wbic[i],]
	n.pairs[i] = nrow(unique(tmp[,list(start, end)]))
	
	if(nrow(unique(tmp[,list(start, end)])) < 200){
		next
	}
	
	tmp[,rel.depth.int:=floor(rel.depth*10)]
	browser()
	
	for(k in 1:10){
		if(nrow(tmp[rel.depth.int==k,]) > 0){
			output[k,j] = median(tmp[rel.depth.int==k,]$slopes, na.rm=TRUE)
		}
	}
	j=j+1
	
}

output = output[1:10,1:j-1]

image(t(output),col=tim.colors(8))

