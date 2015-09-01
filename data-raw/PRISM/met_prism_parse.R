

library(mda.lakes)

prism_mn = load_gdp_file('data-raw/PRISM/prism_tmn.tsv', tz.offset = 0)

prism_mx = load_gdp_file('data-raw/PRISM/prism_tmx.tsv', tz.offset = 0)

lids = names(prism_mn)[-1]

if(!all(lids == names(prism_mx)[-1])){
	stop('not matching tables')
}

for(i in 1:length(lids)){
	lid = lids[i]
	airt = (prism_mn[,lid] + prism_mx[,lid])/2
	to_write = data.frame(time=prism_mn$datetime, AirTemp=airt)
	
	fname = paste0('~/drivers_GLM_PRISM/WBIC_', lid, '.csv')
	write.csv(to_write, fname, quote=FALSE, row.names=FALSE)
}


