## Empirical WTR Trends
library(plyr)
library(nlme)

better.JAS = function(df){
  if(nrow(df) > 4){
    return(mean(df$V1))
  }else{
    return(NA)
  }
}

better.trend = function(df){
  if(nrow(df) > 10 & abs(nrow(df) - (max(df$year) - min(df$year))) < 3){
    return(data.frame(slope=lm(df$V1 ~ df$year)$coeff[2], syear=min(df$year)))
    }else{
      return(data.frame(slope=NA, syear=NA))
  }
}

wtr = read.table('../supporting files/wtemp.obs.tsv', sep='\t', header=TRUE)

wtr.near.surf = ddply(wtr, c("WBIC", "DATETIME"), function(df) mean(df$WTEMP[df$DEPTH<=2], na.rm=TRUE))

mons = as.POSIXlt(wtr.near.surf$DATETIME)$mon+1

wtr.near.surf = wtr.near.surf[mons >=7 & mons <= 9,]
wtr.near.surf$year = as.POSIXlt(wtr.near.surf$DATETIME)$year+1900

JAS.mean = ddply(wtr.near.surf, c("WBIC", "year"), better.JAS)
JAS.mean = JAS.mean[!is.na(JAS.mean$V1),]

slopes = ddply(JAS.mean, c("WBIC"), better.trend)
slopes = slopes[!is.na(slopes$slope), ]

#library(zyp)
#slopes.sens = ddply(JAS.mean, c("WBIC"), function(df) zyp.sen(V1 ~ year, df)$coeff[2])
#tmp = join(slopes,slopes.sens)
#plot(tmp$`df$year`, tmp$year, ylab='sens', xlim=c(-0.5,0.5), ylim=c(-0.5,0.5))

hist(slopes$slope, breaks=10, xlim=c(-1,1))

cat('Median slope all data:', median(slopes$V1, na.rm=TRUE))

slopes.1979 = ddply(JAS.mean[JAS.mean$year>=1979,], c("WBIC"), better.trend )

cat('Median slope since 1979:', median(slopes.1979$V1, na.rm=TRUE))

################################################################################
### Loess through all surface water temps
################################################################################
JAS.mean = JAS.mean[JAS.mean$year >= 1979,]
plot(JAS.mean$year, JAS.mean$V1, xlim=c(1979,2012))

x=1979:2012
ma = loess(V1 ~ year, JAS.mean)
lines(x, predict(ma, x), col='red', lwd=2)

##Predict and plot trend from 1990
from.1990 = JAS.mean[JAS.mean$year >= 1990, ]
jas.lme = lme(V1 ~ year, from.1990, random=~1|WBIC)

fixed.slope = summary(jas.lme)$tTable[2,1]
fixed.yint  = summary(jas.lme)$tTable[1,1]

lines(x, x*fixed.slope+fixed.yint, col='blue', lwd=2)

slopes.1990 = ddply(from.1990, c("WBIC"), function(df) lm(df$V1 ~ df$year)$coeff[2])

##Predict and plot from 1979
from.1979 = JAS.mean[JAS.mean$year >= 1979, ]
jas.lme = lme(V1 ~ year, from.1979, random=~1|WBIC)

################################################################################
### LTER Trends
################################################################################
lter.wbic = read.table('../supporting files/lter.south.lakes.tsv', header=TRUE, sep='\t')
#Generate all.slopes in 'trends.analysis.R'
lter.mod = all.data[all.data$WBIC %in% lter.wbic$WBIC, ]
lter.data = JAS.mean[JAS.mean$WBIC %in% lter.wbic$WBIC,]
lter.data = lter.data[lter.data$year >= 1985 & lter.data$year <= 2011, ]
lter.mod = lter.mod[lter.mod$Year >= 1985 & lter.mod$Year <= 2011, ]


lter.slopes = ddply(lter.data, c("WBIC"), function(df) lm(df$V1 ~ df$year)$coeff[2])
lter.slopes.sens = ddply(lter.data, c("WBIC"), function(df) zyp.sen(V1 ~ year, df)$coeff[2])
lter.mod.slopes = ddply(lter.mod, c("WBIC"), function(df) lm(df$JAS.Mean ~ df$Year)$coeff[2])

names(lter.mod.slopes) = c("WBIC", 'slope.mod')
names(lter.slopes) = c('WBIC', 'slope.empir')


lter.empir.mod = join(lter.mod.slopes, lter.slopes)
lter.empir.mod = join(lter.empir.mod, lter.wbic)

tmp = lter.data[lter.data$WBIC==lter.wbic$WBIC[1],]
tmp.mod = data[data$lakeid==lter.wbic$WBIC[1],]
cat(mean(tmp$V1), '\n')
plot(tmp$year, tmp$V1, type='b', ylim=c(5,30))
lines(tmp.mod$year, tmp.mod$mean_surf_JAS)

for(i in 2:nrow(lter.wbic)){
  tmp = lter.data[lter.data$WBIC==lter.wbic$WBIC[i],]
  tmp.mod = data[data$lakeid==lter.wbic$WBIC[i],]
  
  lines(tmp$year, tmp$V1, type='b')
  cat(mean(tmp$V1), '\n')
  lines(tmp.mod$year, tmp.mod$mean_surf_JAS)
}



