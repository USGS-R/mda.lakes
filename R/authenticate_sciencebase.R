
#'
#'@title Authenticate to sciencebase for subsequent calls
#'
#'
#'
#'
#'
#'
#'
#'@import httr
#'@export
authenticate_sciencebase = function(username, password){
	
	if(!interactive() & missing(password)){
		stop('No password supplied to authenticat_sciencebase in a non-interactive session.')
	}else{
		password = readPassword('Please enter your Sciencebase password:')
	}
	
	#get to nab our cookies
	resp = POST('https://www.sciencebase.gov/catalog/',
							accept_json(),
							authenticate(username, password))
	#TODO: verify that we were authenticated properly
	
}

#'@export
upload_file_to_item = function(id, filename){
	url = 'https://www.sciencebase.gov/catalog/file/uploadAndUpsertItem/'
	
	postForm(url, fileData=fileUpload(filename), id=item, curl=curl, )
	
}

get_item = function(id){
	GET('https://www.sciencebase.gov/catalog/item/50e74a30e4b00c3282564f6d?type=json', config=add_headers(Accept="application/json"))
	
	
	GET('https://www.sciencebase.gov/catalog/item/50e74a30e4b00c3282564f6d?type=json', authenticate('lwinslow@usgs.gov', '', type='basic'), add_headers(Accept="application/json"))
	
}


readPassword <- function(prompt) {
	# found this cool function in rstudio
	if (exists(".rs.askForPassword")) {
		pass <- .rs.askForPassword(prompt)
	} else {
		
		pass <- readline(prompt)
	
	}
	return (pass)
}

	

