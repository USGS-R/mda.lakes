## overall stats

library(data.table)
library(plyr)
source('grl.resample.calcs.R')

all.slopes = data.table(grl.resample.calcs(no.resamp=TRUE))

length(unique(all.slopes$wbic))

median(all.slopes[area > 5e5,]$slopes)
median(all.slopes[area < 5e5,]$slopes)
median(all.slopes$slopes)



median(all.slopes[area > 5e5 & rel.depth < 0.5,]$slopes)
median(all.slopes[area > 5e5 & rel.depth > 0.5,]$slopes)


median(all.slopes[area < 5e5 & rel.depth < 0.5,]$slopes)
median(all.slopes[area < 5e5 & rel.depth > 0.5,]$slopes)
