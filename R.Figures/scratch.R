library(plyr)
library(emdbook)
source('../R/Libraries/GLM.functions.R')

data = read.table('../Output/omg.huge.output.tsv', sep='\t', header=TRUE)

lake.meta= data.frame(WBIC=unique(data$lakeid), max.depth=NA, area=NA, kd=NA)

for(i in 1:nrow(lake.meta)){
  lake.meta$max.depth[i] = getZmax(as.character(lake.meta$WBIC[i]))
  lake.meta$area[i] = getArea(as.character(lake.meta$WBIC[i]))
  if(is.na(lake.meta$max.depth[i])){
    lake.meta$max.depth[i] = max(getBathy(as.character(lake.meta$WBIC[i]))$depth)
  }
  lake.meta$kd[i] = getClarity(as.character(lake.meta$WBIC[i]))
}


air.temp = read.table('../Output/airtemp.metrics.csv', sep=',', header=TRUE)

hyp.temps = read.table('../Output/KdScenarios/stable.metrics.out.tsv', sep='\t', header=TRUE)

all.data = merge(air.temp, data, by.x=c("WBIC", "Year"), by.y=c("lakeid", "year"))

all.data = merge(all.data, hyp.temps, by.x=c("WBIC", "Year"), by.y=c("lakeid", "year"))


r2.slope.fun = function(df){
  tmp = lm(df$mean_surf_JAS ~ df$JAS.Mean)
  slope = tmp$coeff[2]
  names(slope) = NULL
  return(c(r.squared=summary(tmp)$r.squared, slope=slope))
}

air.wtr.slope.r2 = ddply(all.data, c('WBIC'), r2.slope.fun)

  
air.wtr.meta = merge(air.wtr.slope.r2, lake.meta)

# kd vs r.squared
plot(air.wtr.meta$kd, air.wtr.meta$r.squared, xlim=c(0,4))
ma = loess(r.squared ~ kd, air.wtr.meta)
x = seq(0,4,by=0.1)
lines(x, predict(ma, x), col='red', lwd=2)

#kd vs slope
plot(air.wtr.meta$kd, air.wtr.meta$slope, xlim=c(0,4), ylim=c(0.3,1))
ma = loess(slope ~ kd, air.wtr.meta)
x = seq(0.1,4,by=0.1)
lines(x, predict(ma, x), col='red', lwd=2)

# max.depth vs r.squared
plot(air.wtr.meta$max.depth, air.wtr.meta$r.squared, xlim=c(0,40))
ma = loess(r.squared ~ max.depth, air.wtr.meta)
x = seq(0, 40, by=0.1)
lines(x, predict(ma, x), col='red', lwd=2)

# max.depth vs slope
plot(air.wtr.meta$max.depth, air.wtr.meta$slope, xlim=c(0,40), ylim=c(0,1))
ma = loess(slope ~ max.depth, air.wtr.meta)
x = seq(0, 40, by=0.1)
lines(x, predict(ma, x), col='red', lwd=2)


# max.depth vs r.squared
plot(air.wtr.meta$area, air.wtr.meta$r.squared, xlim=c(1, 1e7))
ma = loess(r.squared ~ area, air.wtr.meta)
x = lseq(1, 1e7, length.out=100)
lines(x, predict(ma, x), col='red', lwd=2)

# max.depth vs slope
plot(air.wtr.meta$area, air.wtr.meta$slope, ylim=c(0,1))
ma = loess(slope ~ area, air.wtr.meta)
x = lseq(1, 1e7, length.out=100)
lines(x, predict(ma, x), col='red', lwd=2)

################################################################################
## Now do Hypo!
################################################################################
r2.slope.fun = function(df){
  if(any(is.na(df$mean_hypo_temp))){
    return(c(NA,NA))
  }
  tmp = lm(df$mean_hypo_temp ~ df$JAS.Mean)
  slope = tmp$coeff[2]
  names(slope) = NULL
  return(c(r.squared=summary(tmp)$r.squared, slope=slope))
}

air.hypo.slope.r2 = ddply(all.data, c('WBIC'), r2.slope.fun)
air.hypo.meta = merge(air.hypo.slope.r2, lake.meta)


# kd vs r.squared
plot(air.hypo.meta$kd, air.hypo.meta$r.squared, xlim=c(0,4))
ma = loess(r.squared ~ kd, air.hypo.meta)
x = seq(0,4,by=0.1)
lines(x, predict(ma, x), col='red', lwd=2)

#kd vs slope
plot(air.hypo.meta$kd, air.hypo.meta$slope, xlim=c(0,4), ylim=c(-1,2))
ma = loess(slope ~ kd, air.hypo.meta)
x = seq(0.1,4,by=0.1)
lines(x, predict(ma, x), col='red', lwd=2)

# max.depth vs r.squared
plot(air.hypo.meta$max.depth, air.hypo.meta$r.squared, xlim=c(0,40))
ma = loess(r.squared ~ max.depth, air.hypo.meta)
x = seq(0, 40, by=0.1)
lines(x, predict(ma, x), col='red', lwd=2)

# max.depth vs slope
plot(air.hypo.meta$max.depth, air.hypo.meta$slope, xlim=c(0,40), ylim=c(-1,2))
ma = loess(slope ~ max.depth, air.hypo.meta)
x = seq(0, 40, by=0.1)
lines(x, predict(ma, x), col='red', lwd=2)


# area vs r.squared
plot(air.hypo.meta$area, air.hypo.meta$r.squared, xlim=c(1, 1e7))
ma = loess(r.squared ~ area, air.hypo.meta)
x = lseq(1, 1e7, length.out=100)
lines(x, predict(ma, x), col='red', lwd=2)

# area vs slope
plot(air.hypo.meta$area, air.hypo.meta$slope, ylim=c(-1,2), xlim=c(1, 1e7))
ma = loess(slope ~ area, air.hypo.meta)
x = lseq(1, 1e7, length.out=100)
lines(x, predict(ma, x), col='red', lwd=2)





## Some stuff on sheltering

source('../R/Libraries/GLM.functions.R')
library(data.table)
library(plyr)
all.slopes = fread('all.slopes.csv')

#u.wbic = unique(all.slopes$wbic)
#wst    = rep(NA, length(u.wbic))
#for(i in 1:length(u.wbic)){
#  wst[i] = getWstr(as.character(u.wbic[i]), method='Hondzo')
#}

#wst = data.frame(wbic=u.wbic, wst=wst)
#tmp = merge(all.slopes, wst, by='wbic')

all.slopes = all.slopes[,rel.depth:=floor(10*depth/zmax)/10]
all.slopes = all.slopes[rel.depth <= 1,]

tmp = all.slopes
tmp[,wst:=(1.0 - exp(-0.3*area*1e-6))]
tmp[,wst3:=wst^3]


area.wst = unique(tmp[,list(wbic,area,wst, wst3,kd)])

plot(wst~area, area.wst, log='x', ylim=c(0,2), xlim=c(5e3,5e8))
abline(v=5e5, lwd=2)
lines(sort(area.wst$area), sort(area.wst$wst3))

wbic.slope = ddply(all.slopes[rel.depth>=0.5,],'wbic',function(df)c(median(df$area),median(df$slopes), nrow(df), median(df$kd)))

#wbic.slope = ddply(all.slopes,'wbic',
#                   function(df)c(median(df$area),
#                                 median(df[df$rel.depth<0.5,]$slopes) - median(df[df$rel.depth>0.5,]$slopes), 
#                                 nrow(df), median(df$kd)))

wbic.slope = wbic.slope[wbic.slope$V3 >=3000,]

par(new=TRUE)
plot(wbic.slope$V1, wbic.slope$V2, col='blue', pch=16,
     ylim=c(0,0.4),xlim=c(5e3,5e8), log='x', )



wbic.slope = ddply(all.slopes[rel.depth<0.5,],'wbic',function(df)c(median(df$area),median(df$slopes), nrow(df), median(df$kd)))

wbic.slope = wbic.slope[wbic.slope$V3 > 4000,]

par(new=TRUE)
plot(wbic.slope$V1, wbic.slope$V2, col='red', pch=16,
     ylim=c(0,0.4),xlim=c(5e3,5e8), log='x', )


