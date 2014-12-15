
#'@importFrom plyr ddply
#'@export
load_gdp_file = function(fname, tz.offset=0){
	
	if(!require(lubridate)){
		stop('need to install lubridate for load_gdp_file')
	}
	
	## open file
	fid = file(fname, open='r')
	# drop the first line, just variable name 
	discard = readLines(fid, 1)
	# first line is lake ids (WBIC in our case)
	ids = strsplit(readLines(fid, 1), '\t', fixed=T)[[1]]
	
	#read the full data table
	data = read.table(fid, sep='\t', header=TRUE, as.is=TRUE)
	close(fid)
	
	#parse the date into POSIXct format
	data$TIMESTEP = parse_date_time2(data$TIMESTEP, 'YmdHMS') + tz.offset*60*60
	
	ids[1] = 'datetime'
	names(data) = ids
	return(data)
}
