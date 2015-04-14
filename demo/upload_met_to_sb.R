

upload_met_to_sb = function(met_source, lake_ids, file_paths, session){
	
	if(!require(sbtools)){
		stop('sbtools must be installed to interact with SB')
	}
	
	if(length(lake_ids) != length(file_paths)){
		stop('lake_ids and file_paths must be the same length')
	}
	
	root_id = query_item_identifier('mda_lakes', 'project_root', met_source)
	
	if(nrow(root_id) != 1){
		stop('issue querying root node, mda_lakes:project_root:', met_source)
	}
	
	root_id = root_id$id
	
	for(i in 1:length(lake_ids)){
		
		cat('uploading', lake_ids[i], '\n')
		item_id = query_item_identifier('mda_lakes', met_source, lake_ids[i])
		
		if(nrow(item_id) > 0){
			#TODO: Do something if exists
		}
		
		new_id = item_create(root_id, title=lake_ids[i], session=session)
		
		item_update_identifier(new_id, 'mda_lakes', met_source, lake_ids[i], session=session)
		item_append_file(new_id, file_paths[i], session=session)
	}
}

library(stringr)

files = Sys.glob('F:\\WiLMA_drivers\\07-30-appended2013\\*.csv')

root_id = '54d8fc63e4b0f7b2dc9f42fc'

lake_ids = str_extract(basename(files), 'WBIC_[0-9]+')

met_source = 'met_nldas'

session = authenticate_sb('lwinslow@usgs.gov')
upload_met_to_sb(met_source, lake_ids, files, session)


