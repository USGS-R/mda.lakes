#'@title Get light attenuation coefficient for a lake through time
#'@description
#'Returns the annual average light attenuation coefficient for a lake with the given ID
#'
#'@param site_id The character ID for the requested data
#'@param years Integer vector of years for which you want Kd values
#'
#'@return
#' Vector of Kd values with the same length as \code{years}
#'@details
#' TODO
#'
#'
#'@author 
#'Luke Winslow, Jordan Read
#'
#'@examples
#'#Get and plot Kds for a lake over time
#'years = 1970:2012
#'kds   = get_kd_year('6100', years)
#'plot(years, kds, type='o')
#'
#'
#'@export
get_kd_year = function(site_id, years){
	
	if(missing(years)){
		stop('You must supply one or more years')
	}
	
	secchiConv	<-	1.7
	
	fname = system.file('supporting_files/annual_mean_secchi.txt', package=packageName())
	d	    = read.table(fname, header=TRUE, sep='\t', as.is=TRUE)
	
	site_secchi = merge(data.frame(site_id, year=years, stringsAsFactors=FALSE),
											d[,c('WBIC', 'year', 'secchi.m.mean')], all.x=TRUE, by.x=c('site_id', 'year'), by.y=c('WBIC', 'year'))
	
	site_secchi = site_secchi[order(site_secchi$year), ]
	kd = secchiConv/site_secchi$secchi.m.mean
	
	#return kd
	return(kd)
}