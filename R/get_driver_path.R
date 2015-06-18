
#'@title Return driver file location for given lake
#'
#'@description
#'This functoin returns a path to the driver file for a lake. Retrieve and cache
#'the file from Sciencebase if necessary
#'
#'@param fname Name of the file, generally based on the unique site_id
#'@param driver_name The driver source name, options ('NLDAS', 'CM2.0', 'GENMOM', 'ECHAM5')
#'
#'@author Luke Winslow
#'
#'
#'@export
get_driver_path = function(fname, driver_name='NLDAS', loc_cache=TRUE){
	
	base_url = 'http://cida-test.er.usgs.gov/mda.lakes/'
	
	full_url = paste0(base_url, 'drivers_GLM_', driver_name, '/', fname)
	dest = file.path(tempdir(), driver_name, fname)
	
	if(file.exists(dest) && loc_cache){
		return(dest)
	} 
		
	if(!file.exists(dirname(dest))){
		dir.create(dirname(dest), recursive=TRUE)
	}
	
	status = 1
	#catch the download file error, and give more descriptive error
	tryCatch({
		status = download.file(full_url, destfile = dest, quiet=TRUE)
	}, error=function(e){})
	
	#loc = unzip(driver_path, files=fname, exdir=dest)
	
	if(status != 0){
		stop(fname, ':file of that name does not exist for driver:', driver_name)
	}
	
	return(dest)
}

#@importFrom jsonlite fromJSON
#@importFrom RCurl getBinaryURL
# get_driver_path = local({ lookup=NULL; function(fname){
#   
#   if(is.null(lookup)){
#     lookup <<- new.env()
#     scibase_json = 'https://www.sciencebase.gov/catalog/item/543c2f9be4b0fd76af69c8c5?format=json'
#     
#     scibase_item = fromJSON(scibase_json)
#     #ids = str_extract(tmp$files$name, '[0-9]+')
#     filemap = data.frame(fnames=scibase_item$files$name,
#                          urls=scibase_item$files$url, local_path='',
#                          stringsAsFactors=FALSE)
#     
#     ##Do this twice because we had to split the drivers
#     #scibase_json = 'https://www.sciencebase.gov/catalog/item/542dc7e6e4b092f17defcb06?format=json'
#     #scibase_item = fromJSON(scibase_json)
#     
#     #filemap = rbind(filemap, 
#      #               data.frame(fnames=scibase_item$files$name,
#       #                         urls=scibase_item$files$url, local_path='',
#        #                        stringsAsFactors=FALSE))
#     
#     
#     lookup[['filemap']] = filemap
#   }
#   
#   filemap = lookup[['filemap']]
#   
#   index = which(filemap$fnames %in% fname)
#   
#   #If sciencebase doesn't have the file, error out
#   if(length(index) <= 0){
#     ##TODO: Give user the option to just check for file existence
#     stop('no file of name: ', fname)
#   }
#   
#   #
#   if(filemap[index, 'local_path'] == ''){
#     #download file into temporary location
#     tmp_path = tempfile(fname, fileext = '.csv')
#     bin = getBinaryURL(filemap[index,'urls'], ssl.verifypeer = FALSE) 
#     writeBin(bin, tmp_path)
#     filemap[index, 'local_path'] = tmp_path
#     lookup[['filemap']] = filemap
#   }
#   
#   filemap[index, 'local_path']
# }})



