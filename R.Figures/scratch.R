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





