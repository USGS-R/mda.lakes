library(plyr)
library(dplyr)
library(rLakeAnalyzer)
library(reshape2)
library(lubridate)
#Analyze!

load('~/FUTURE_GENMOM.Rdata')
site_ids = unlist(lapply(dframes, function(l){l$site_id[1]}))

lake_hab = read.table('~/2015-06-15_THE_FUTURE_CM2.0.tsv', header=TRUE, sep='\t')


bathy = getBathy('805400')
names(bathy) = c('depths', 'areas')

tmp = filter(lake_hab, site_id=='WBIC_805400')

plot(tmp$year, tmp$durStrat)
plot(tmp$year, tmp$mean_surf_jul)

mean(tmp$mean_surf_JAS[tmp$year %in% 1989:1999])
mean(tmp$mean_surf_JAS[tmp$year %in% 2059:2069])



tmp = dframes[[which(site_ids=='WBIC_805400')]]
tmp$site_id = NULL
tmp$DateTime = as.POSIXct(tmp$DateTime)
names(tmp) = tolower(names(tmp))

#wtr.heat.map(tmp)

int_eng = ts.internal.energy(tmp, bathy)
int_eng_sens = sens_seasonal_site(year(int_eng$datetime), int_eng$internal.energy, season_i = yday(int_eng$datetime))

boxplot(int_eng_sens$slopes, ylim=c(-1e7,1e7))
abline(0, 0)

long = melt(tmp, id.vars='datetime', value.name = 'temp', variable.name = 'depth', factorsAsStrings=TRUE)
long$depth = as.numeric(sapply(as.character(long$depth), function(x){substr(x,5,nchar(x))}))


wtr_sens = sens_seasonal_site(year(long$datetime), long$temp, yday(long$datetime), long$depth)

boxplot(wtr_sens$slopes, ylim=c(-0.5, 0.5))
abline(0,0)

png('~/season.sens.mendota.png', res=300, width=2000, height=1500)
boxplot(slopes~season_i, wtr_sens, ylim=c(-0.1, 0.1))
abline(0,0)
dev.off()

png('~/depths.sens.mendota.png', res=300, width=2000, height=1500)
boxplot(slopes~sites_i, wtr_sens, ylim=c(-0.1, 0.1))
abline(0,0)
dev.off()


