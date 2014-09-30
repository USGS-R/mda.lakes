##asdf
library(data.table)
library(plyr)
source("../R/Libraries/GLM.functions.R")
ice = fread('../supporting files/ice.obs.tsv')

ice = ice[on.off=='off',]

slopes = ddply(ice, 'wbic', function(df) coefficients(lm(doy~ice.year,df))[2])
slopes$area = NA

for(i in 1:nrow(slopes)){
	area = getArea(as.character(slopes$wbic[i]))
	if(!is.null(area)){
		slopes$area[i] = area
	}
}
