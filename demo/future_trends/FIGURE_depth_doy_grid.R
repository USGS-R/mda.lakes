# Lets make a stratification heat map
library(mda.lakes)
library(plyr)
library(dplyr)
library(rLakeAnalyzer)
library(reshape2)
library(lubridate)
#Analyze!

load('D:/BigDatasets/WiLMA/FUTURE_GENMOM.Rdata')
site_ids = unlist(lapply(dframes, function(l){l$site_id[1]}))

bathy = getBathy('1842400')
names(bathy) = c('depths', 'areas')
rm(dframes)

tmp = dframes[[which(site_ids=='WBIC_1842400')]]
tmp$site_id = NULL
tmp$DateTime = as.POSIXct(tmp$DateTime)
names(tmp) = tolower(names(tmp))
long = melt(tmp, id.vars='datetime', value.name = 'temp', variable.name = 'depth', factorsAsStrings=TRUE)
long$depth = as.numeric(sapply(as.character(long$depth), function(x){substr(x,5,nchar(x))}))


wtr_sens = sens_seasonal_site(year(long$datetime), long$temp, yday(long$datetime), long$depth)

grid_df = ddply(wtr_sens, c('sites_i', 'season_i'), function(df){data.frame(slope=median(df$slopes, na.rm=TRUE), num=nrow(df))})

grid_df = grid_df[grid_df$num > 1000,]

grid_mat = dcast(grid_df, sites_i~season_i, median, value.var='slope')

y = grid_mat[,1]
x = as.numeric(names(grid_mat)[-1])

png('~/grid.sens.me.png', res=300, width=2000, height=1500)
image(x, y, t(as.matrix(grid_mat[,-1])), ylim=rev(range(y)), col=rev(heat.colors(12)))
dev.off()

png('~/season.sens.me.png', res=300, width=2000, height=1500)
boxplot(slopes~season_i, wtr_sens, ylim=c(-0.1, 0.1))
abline(0,0)
dev.off()

png('~/depths.sens.me.png', res=300, width=2000, height=1500)
boxplot(slopes~sites_i, wtr_sens, ylim=c(-0.1, 0.1))
abline(0,0)
dev.off()

