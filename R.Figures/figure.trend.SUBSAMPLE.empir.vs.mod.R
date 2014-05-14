## Figure to compare empirical vs modeled trends
library(plyr)
library(nlme)

better.JAS = function(df, col){
  if(nrow(df) > 4){
    return(mean(df[,col]))
  }else{
    return(NA)
  }
}

better.trend = function(df, col){
  if(nrow(df) > 10 & abs(nrow(df) - (max(df$year) - min(df$year))) < 3){
    return(data.frame(slope=lm(df[,col] ~ df$year)$coeff[2], syear=min(df$year)))
  }else{
    return(data.frame(slope=NA, syear=NA))
  }
}

wtr = read.table('D:/WilmaRuns/2014-05-08multipliers/v1.2lwMultip1/all.cal.tsv', sep='\t', header=TRUE)

cal.near.surf = ddply(wtr, c("WBIC", "DATETIME"), function(df) mean(df$WTEMP_MOD[df$DEPTH<=3], na.rm=TRUE))
wtr.near.surf = ddply(wtr, c("WBIC", "DATETIME"), function(df) mean(df$WTEMP[df$DEPTH<=3], na.rm=TRUE))

names(cal.near.surf)[3] = 'CAL'
#cal.near.surf = cal.near.surf[!is.na(cal.near.surf$CAL), ]
wtr.near.surf = merge(wtr.near.surf, cal.near.surf, by=c('WBIC', 'DATETIME'))

mons = as.POSIXlt(wtr.near.surf$DATETIME)$mon+1

wtr.near.surf = wtr.near.surf[mons >=7 & mons <= 9,]
wtr.near.surf$year = as.POSIXlt(wtr.near.surf$DATETIME)$year+1900
good.wbic = c(30300, 287200, 804600, 805000, 805400, 1593800, 1835300, 1842400, 1881900, 2331600, 2332400)
to.plot = wtr.near.surf[wtr.near.surf$WBIC == good.wbic[1], ]
to.plot = to.plot[order(as.POSIXct(to.plot$DATETIME)), ]

star =as.POSIXct('1996-01-01')
end  =as.POSIXct('1997-01-01')

tiff(paste('../Figures/', good.wbic[1], '.tiff', sep=''))
plot(as.POSIXct(to.plot$DATETIME), to.plot$V1, type='o', xlim=c(star,end), ylim=c(0,26))
lines(as.POSIXct(to.plot$DATETIME), to.plot$CAL, col='red')
dev.off()
for(i in 2:length(good.wbic)){
	to.plot = wtr.near.surf[wtr.near.surf$WBIC == good.wbic[i], ]
	to.plot = to.plot[order(as.POSIXct(to.plot$DATETIME)), ]
	tiff(paste('../Figures/', good.wbic[i], '.tiff', sep=''))
	plot(as.POSIXct(to.plot$DATETIME), to.plot$V1, type='o', xlim=c(star,end), ylim=c(0,26), main=good.wbic[i])
	lines(as.POSIXct(to.plot$DATETIME), to.plot$CAL, col='red')
	dev.off()
}

cal.JAS.mean = ddply(wtr.near.surf, c("WBIC", "year"), function(df) better.JAS(df, col='CAL'))
cal.JAS.mean = cal.JAS.mean[!is.na(cal.JAS.mean$V1),]

JAS.mean = ddply(wtr.near.surf, c("WBIC", "year"), function(df) better.JAS(df, col='V1'))
JAS.mean = JAS.mean[!is.na(JAS.mean$V1),]

slopes = ddply(JAS.mean, c("WBIC"), function(df) better.trend(df,col='V1'))
slopes = slopes[!is.na(slopes$slope), ]
names(slopes)[2] = 'obs.slope'
	
cal.slopes = ddply(cal.JAS.mean, c("WBIC"), function(df) better.trend(df,col='V1'))
cal.slopes = cal.slopes[!is.na(cal.slopes$slope), ]
names(cal.slopes)[2] = 'cal.slope'

both.slopes = merge(cal.slopes,slopes,by=c('WBIC','syear'))

tiff('../Figures/JAS.trends.lter.mod.SUBSAMP.tiff', width=1600, height=1600, res=300, compression='lzw')
plot(both.slopes$obs.slope, both.slopes$cal.slope, xlim=c(0,0.1), ylim=c(0,0.1),
		 xlab='Slope JAS Obs', ylab='Slope JAS Cal', pch=16, col='#00000070')
abline(0,1, lwd=2)
dev.off()

stop('below unecessary')


lter.wbic = read.table('../supporting files/lter.south.lakes.tsv', header=TRUE, sep='\t')
lter.wbic$region = "South"


lter.data = JAS.mean[JAS.mean$WBIC %in% lter.wbic$WBIC,]
lter.data = lter.data[lter.data$year >= 1995 & lter.data$year <= 2011, ]

lter.mod.slopes = ddply(lter.data, c("lakeid"), function(df) lm(df$mean_epi_temp ~ df$year)$coeff[2])
lter.mod.slopes.sens = ddply(lter.data, c("lakeid"), function(df) zyp.sen(mean_epi_temp ~ year, df)$coeff[2])

lter.slopes = ddply(lter.data, c("WBIC"), function(df) lm(df$V1 ~ df$year)$coeff[2])
lter.slopes.sens = ddply(lter.data, c("WBIC"), function(df) zyp.sen(V1 ~ year, df)$coeff[2])


names(lter.mod.slopes.sens) = c("WBIC", 'slope.mod')
names(lter.slopes.sens) = c('WBIC', 'slope.empir')


lter.empir.mod = join(lter.mod.slopes.sens, lter.slopes.sens)
lter.slopes.south = join(lter.empir.mod, lter.wbic)

lter.wbic = read.table('../supporting files/lter.north.lakes.tsv', header=TRUE, sep='\t')
lter.wbic$region = "North"
epi.temps = read.table('../Output/KdScenarios/stable.metrics.out.tsv', sep='\t', header=TRUE)

#lter.mod = data[data$lakeid %in% lter.wbic$WBIC, ]
lter.mod = epi.temps[epi.temps$lakeid %in% lter.wbic$WBIC,]
lter.mod = lter.mod[lter.mod$year >= 1985 & lter.mod$year <= 2011, ]

lter.data = JAS.mean[JAS.mean$WBIC %in% lter.wbic$WBIC,]
lter.data = lter.data[lter.data$year >= 1985 & lter.data$year <= 2011, ]

lter.mod.slopes = ddply(lter.mod, c("lakeid"), function(df) lm(df$mean_epi_temp ~ df$year)$coeff[2])
lter.mod.slopes.sens = ddply(lter.mod, c("lakeid"), function(df) zyp.sen(mean_epi_temp ~ year, df)$coeff[2])

lter.slopes = ddply(lter.data, c("WBIC"), function(df) lm(df$V1 ~ df$year)$coeff[2])
lter.slopes.sens = ddply(lter.data, c("WBIC"), function(df) zyp.sen(V1 ~ year, df)$coeff[2])


names(lter.mod.slopes.sens) = c("WBIC", 'slope.mod')
names(lter.slopes.sens) = c('WBIC', 'slope.empir')


lter.empir.mod = join(lter.mod.slopes.sens, lter.slopes.sens)
lter.slopes.north = join(lter.empir.mod, lter.wbic)

lter.slopes = rbind(lter.slopes.north, lter.slopes.south)


tiff('NTL.slopes.tiff', width=1200, height=1200, res=300, compression='lzw')
boxplot(slope.empir ~ region, data=lter.slopes, lwd=2, ylab='JAS Trend', xlab='NTL Region')
dev.off()

tiff('mod.v.empir.trends.tiff', width=1200, height=1200, res=300, compression='lzw')
plot(lter.slopes$slope.empir, lter.slopes$slope.mod, 
     xlab="Empirical trends", ylab="Modeled trends", pch=16, xlim=c(-0.03, 0.18), ylim=c(-0.03, 0.18))
lines(c(-1,1), c(-1,1), lwd=2)
dev.off()



