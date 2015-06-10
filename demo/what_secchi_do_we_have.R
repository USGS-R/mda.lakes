
#What do we have?

library(lakeattributes)
library(mda.lakes)
library(dplyr)

insitu_secchi = filter(secchi, source=='in-situ')
	
#secchi and turbidity
secchi_bare = substr(insitu_secchi$site_id, 6, nchar(insitu_secchi$site_id))
turb_bare = substr(turbidity$site_id, 6, nchar(site_id))

#managed lakes
fpath = system.file('supporting_files/managed_lake_info.txt', package='mda.lakes')
managed_lakes = read.table(fname, header=TRUE, sep='\t', quote="\"", colClasses=c(WBIC='character'))

#wtemp_obs
obs = read.table(system.file('supporting_files/wtemp.obs.tsv', package = 'mda.lakes'), 
								 sep='\t', header=TRUE, as.is=TRUE, colClasses=c(WBIC='character'))


intersect(managed_lakes$WBIC, secchi_bare)

intersect(obs$WBIC, secchi_bare)



insitu_latlon = data.frame(site_id=unique(insitu_secchi$site_id), lat=NA, lon=NA, stringsAsFactors = FALSE)
insitu_latlon$WBIC = substr(insitu_latlon$site_id, 6, nchar(insitu_latlon$site_id))

for(i in 1:nrow(insitu_latlon)){
	
	tmp = getLatLon(insitu_latlon$WBIC[i])
	if(!is.null(tmp)){
		insitu_latlon$lat[i] = tmp[1]
		insitu_latlon$lon[i] = tmp[2]
	}
}


