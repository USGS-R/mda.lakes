

get_driver_path = local({ lookup=NULL; function(id){
	require(jsonlite)
	require(stringr)
	
	if(is.null(lookup)){
		lookup <<- new.env()
		scibase_json = 'https://www.sciencebase.gov/catalog/item/51af6fd4e4b08a3322c44897?format=json'
		
		ids = str_extract(tmp$files$name, '[0-9]+')
		data.frame(ids=ids, fnames=tmp$files$name, urls=tmp$files$url)
		
		
		
	}
	
	
	
	
	
	
}