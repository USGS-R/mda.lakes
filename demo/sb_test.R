library(httr)

username = 'lwinslow@usgs.gov'
pass <- .rs.askForPassword(prompt)

url_base = "https://www.sciencebase.gov/catalog/"
url_items = paste0(url_base, "items/")
url_item = paste0(url_base, "item/")
url_upload = paste0(url_base, 'file/uploadAndUpsertItem/')
url_login = 'https://my.usgs.gov/josso/signon/usernamePasswordLogin.do'
item = '543c2f9be4b0fd76af69c8c5'

to_upload = 'D:/test.txt'


add_headers(Accept="application/json")


h = handle(url_base)

## authenticate
resp = GET(url_base, accept_json(), authenticate(username, pass, type='basic'),
		 		handle=h)

sid = resp$cookies$JSESSIONID

resp = GET(paste0(url_item, item, '?type=json'), accept_json(), 
					 verbose(), handle=h)


to_upload = Sys.glob('D:/WiLMA_drivers/07-30-appended2013/*.csv')

for(i in 1:length(to_upload)){
	POST(paste0(url_upload,'?id=', item), accept_json(), 
			 body=list(file=upload_file(to_upload[i])), handle=h)
}



rm(pass)