#Look at JA trends compared to LTER
## Figure to compare empirical vs modeled trends
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

wtr.near.surf = ddply(wtr, c("WBIC", "DATETIME"), function(df) mean(df$WTEMP[df$DEPTH<=5], na.rm=TRUE))

mons = as.POSIXlt(wtr.near.surf$DATETIME)$mon+1

wtr.near.surf = wtr.near.surf[mons >=7 & mons <= 8,]
wtr.near.surf$year = as.POSIXlt(wtr.near.surf$DATETIME)$year+1900

JAS.mean = ddply(wtr.near.surf, c("WBIC", "year"), better.JAS)
JAS.mean = JAS.mean[!is.na(JAS.mean$V1),]

slopes = ddply(JAS.mean, c("WBIC"), better.trend)
slopes = slopes[!is.na(slopes$slope), ]

data = read.table('../Output/omg.huge.output.tsv', sep='\t', header=TRUE)


lter.wbic = read.table('../supporting files/lter.south.lakes.tsv', header=TRUE, sep='\t')
lter.wbic$region = "South"
epi.temps = read.table('../Output/KdScenarios/stable.metrics.out.tsv', sep='\t', header=TRUE)

#lter.mod = data[data$lakeid %in% lter.wbic$WBIC, ]
lter.mod = epi.temps[epi.temps$lakeid %in% lter.wbic$WBIC,]
lter.mod = lter.mod[lter.mod$year >= 1995 & lter.mod$year <= 2011, ]

lter.data = JAS.mean[JAS.mean$WBIC %in% lter.wbic$WBIC,]
lter.data = lter.data[lter.data$year >= 1995 & lter.data$year <= 2011, ]

lter.mod.slopes = ddply(lter.mod, c("lakeid"), function(df) lm(df$mean_surf_JA ~ df$year)$coeff[2])
lter.mod.slopes.sens = ddply(lter.mod, c("lakeid"), function(df) zyp.sen(mean_surf_JA ~ year, df)$coeff[2])

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

lter.mod.slopes = ddply(lter.mod, c("lakeid"), function(df) lm(df$mean_surf_JA ~ df$year)$coeff[2])
lter.mod.slopes.sens = ddply(lter.mod, c("lakeid"), function(df) zyp.sen(mean_surf_JA ~ year, df)$coeff[2])

lter.slopes = ddply(lter.data, c("WBIC"), function(df) lm(df$V1 ~ df$year)$coeff[2])
lter.slopes.sens = ddply(lter.data, c("WBIC"), function(df) zyp.sen(V1 ~ year, df)$coeff[2])


names(lter.mod.slopes.sens) = c("WBIC", 'slope.mod')
names(lter.slopes.sens) = c('WBIC', 'slope.empir')


lter.empir.mod = join(lter.mod.slopes.sens, lter.slopes.sens)
lter.slopes.north = join(lter.empir.mod, lter.wbic)

lter.slopes = rbind(lter.slopes.north, lter.slopes.south)


tiff('../Figures/NTL.JA.slopes.tiff', width=1200, height=1200, res=300, compression='lzw')
boxplot(slope.empir ~ region, data=lter.slopes, lwd=2, ylab='JAS Trend', xlab='NTL Region')
dev.off()

tiff('../Figures/mod.v.empir.JA.trends.tiff', width=1200, height=1200, res=300, compression='lzw')
plot(lter.slopes$slope.empir, lter.slopes$slope.mod, 
		 xlab="Empirical trends", ylab="Modeled trends", pch=16, xlim=c(-0.03, 0.18), ylim=c(-0.03, 0.18))
abline(0, 1, lwd=2)
dev.off()

