source('sens.confint.mod.R')
library(plyr)

all.slopes = fread('all.slopes.csv')
all.slopes = all.slopes[,rel.depth:=floor(10*depth/zmax)/10]
all.slopes = all.slopes[rel.depth <= 1,]


u.wbic = unique(all.slopes$wbic)
n.slopes = ddply(all.slopes,'wbic',function(df)nrow(df))
names(n.slopes) = c('wbic','nnn')
n.slopes = n.slopes[order(n.slopes$nnn, decreasing=TRUE),]

u.wbic = n.slopes$wbic



for(i in 1:length(u.wbic)){
	tiff(paste('try/', i, '.tiff'))
	medians = ddply(all.slopes[wbic==u.wbic[i]],'rel.depth', function(df)median(df$slopes))
	
	
	title = paste(floor(all.slopes[wbic==u.wbic[i],][1,]$area/1e5)/10, u.wbic[i])
	
	boxplot(slopes~rel.depth, all.slopes[wbic==u.wbic[i]], ylim=c(-0.2,0.2), main=title)
	abline(0,0)
	dev.off()
}

