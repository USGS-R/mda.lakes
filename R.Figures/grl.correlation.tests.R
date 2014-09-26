library(data.table)
library(plyr)
source('../R/Libraries/GLM.functions.R')
all.slopes = fread('all.slopes.csv')


tmp = ddply(all.slopes, 'wbic', function(df)c(median(df$slopes), nrow(df)))
median(tmp$V1)
meta = fread('../supporting files/managed_lake_info.txt')

more = tmp[tmp$V2 > 400,]

test = merge(more, meta, by.x='wbic',by.y='WBIC')

getWstr.Markfort = getWstr
getWstr.Hondzo	 = getWstr

test$kd   = NA
test$wstr = NA
test$wstr.h = NA
test$area = NA

for(i in 1:nrow(test)){
	clar = getClarity(as.character(test$wbic[i]))
	test$kd[i]   = clar
	test$wstr[i] = getWstr.Markfort(as.character(test$wbic[i]), method='Markfort')
	test$wstr.h[i] = getWstr.Hondzo(as.character(test$wbic[i]), method='Hondzo')
	test$area[i] = getArea(as.character(test$wbic[i]))
}


#all correlat significantly
cor.test(log10(test$area), test$V1)
cor.test(log10(test$wstr), test$V1)
cor.test((test$wstr^3), test$V1)


#no significant correlation
cor.test((test$max.depth.ft), test$V1)
cor.test((test$kd), test$V1)



#segmented regression
library(segmented)

test$log10_acres = log10(test$acres)

lm.area.slope = lm(slopes~area, all.slopes)

segmented(lm.area.slope, seg.Z=~area, psi=1e6)

