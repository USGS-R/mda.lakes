## Ice on/off trends
library(data.table)
library(nlme)
library(zyp)
library(plyr)

ice = fread('../supporting files/ice.obs.tsv')
setkey(ice, on.off, wbic)

ice.mod = read.table('../supporting files/empirical.ice.tsv', sep='\t', header=TRUE)
names(ice.mod) = tolower(names(ice.mod))
ice.mod = data.table(ice.mod)
setkey(ice.mod, on.off, wbic)


## Ice Off Trends
lme(doy ~ ice.year, ice.mod["off",], ~1|wbic)
median(ddply(ice.mod["off"], 'wbic', function(df) zyp.sen(doy ~ ice.year, df)$coeff[2])[,2])

## Ice On Trends
lme(doy ~ ice.year, ice["off",], ~1|wbic)
median(ddply(ice["off"], 'wbic', function(df) zyp.sen(doy ~ ice.year, df)$coeff[2])[,2])

stop('just stop here, the rest corroborate off gets earlier')


ice.both = merge(ice, ice.mod, by=c("wbic", "ice.year", "on.off"), all.x=TRUE)

#obs - mod
ice.both[,resid:=doy.x-doy.y]
ice.both = ice.both[!is.na(resid) & !is.na(ice.year),]
ice.both = ice.both[, ice.year:=as.numeric(ice.year)]
ice.both = ice.both[, resid:=as.numeric(resid)]

zyp.sen(resid~ice.year, ice.both)
confint(lm(resid~ice.year, ice.both))


## Misc code

median(ddply(ice["off"], 'wbic', function(df) zyp.sen(doy ~ ice.year, df)$coeff[2])[,2])


plot(ice["off"]$ice.year,ice["off"]$doy)

(ddply(ice["off"], 'wbic', function(df) abline(lm(doy ~ ice.year, df))))


all.slopes.perms = function(data){
	
	perm.i = combn(1:nrow(data),2)
	perm1 = data[perm.i[1,],]
	perm2 = data[perm.i[2,],]
	
	return((perm1[,2]-perm2[,2])/(perm1[,1] - perm2[,1]))
}

all.slopes = c()
uwbic = unique(ice$wbic)

for(i in 1:length(uwbic)){
	tmp = ice[wbic==uwbic[i] & on.off=="off",]
	all.slopes = c(all.slopes, all.slopes.perms(data.frame(tmp[,list(ice.year,doy)])))
}

median(all.slopes, na.rm=TRUE)



