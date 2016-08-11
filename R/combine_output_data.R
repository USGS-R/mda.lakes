#library(mda.lakes)
#library(sbtools)
#library(jsonlite)

#' @title combine full sim run output data
#' 
#' @description Combines all the individual compute node model files into 
#' a few files for the whole simulation
#' 
#' 
#' 
#' @export
combine_output_data = function(sim, path){
  #fast temp location for bundling wtr out data
  #this needs about 60GB of scratch space available
  fast_tmp = tempdir()
  
  #ensure we have a trailing / on path
  if(!substr(path, nchar(path), nchar(path)) == '/'){
    path = paste0(path, '/')
  }
  
  core_path = paste0(path, sim, '/', sim, '_core_metrics.tsv')
  cfg_path = paste0(path, sim, '/', sim, '_model_config.json')
  hansen_path = paste0(path, sim, '/', sim, '_fish_hab.tsv')
  
  core_metrics = comb_output_table(paste0(path, sim, '/*/best_core_metrics.tsv'), 
                                   sep='\t', header=TRUE, as.is=TRUE)
  
  write.table(core_metrics, core_path, 
              sep='\t', row.names=FALSE)
  
  hab_metrics =  comb_output_table(paste0(path, sim, '/*/best_hansen_hab.tsv'), 
                                   sep='\t', header=TRUE, as.is=TRUE)
  write.table(hab_metrics, hansen_path, 
              sep='\t', row.names=FALSE)
  
  nml_files = Sys.glob(paste0(path, sim, '/*/model_config.Rdata*'))
  
  ###read and handle NML files
  all_nml = list()
  for(i in 1:length(nml_files)){
    load(nml_files[i])
    all_nml = c(all_nml, model_config)
  }
  
  all_nml = lapply(all_nml, function(x){class(x)='list'; x})
  #save('all_nml', file = paste0(path, sim, '/model_config.Rdata'))
  writeLines(toJSON(all_nml), cfg_path)
  
  ###read and handle raw water temp data.
  wtr_files = Sys.glob(paste0(path, sim, '/*/best_all_wtr.Rdata*'))
  
  all_wtr_files = c()
  wtemp_dir = file.path(fast_tmp, sim)
  dir.create(wtemp_dir)
  
  for(i in 1:length(wtr_files)){
    load(wtr_files[i])
    
    newfiles = lapply(dframes, function(df){
      site_id = df$site_id[1]
      df$site_id = NULL
      wtemp_path = paste0(wtemp_dir, '/', sim, '_', site_id, '.tsv')
      
      #the future sim periods were done separately, so they need to be appended
      if(wtemp_path %in% all_wtr_files){
        write.table(df, wtemp_path, sep='\t', row.names=FALSE, quote=FALSE, append=TRUE, col.names=FALSE)
      }else{
        write.table(df, wtemp_path, sep='\t', row.names=FALSE, quote=FALSE)
      }
      
      return(wtemp_path)
    })
    
    all_wtr_files = c(all_wtr_files, newfiles)
  }
  
  #split up files into 1000 lake groups 
  all_wtr_files = sort(unique(unlist(all_wtr_files)))
  splits = split(1:length(all_wtr_files), floor((1:length(all_wtr_files))/500))
  
  wtemp_zips = file.path(path, sim, paste0(sim, '_wtemp_', seq_along(splits), '.zip'))
  
  #write an index file for later users
  wtemp_zip_index = do.call(rbind, lapply(seq_along(splits), function(i){
    data.frame(file_index=rep(basename(wtemp_zips)[i], length(splits[[i]])), 
               file_name=basename(all_wtr_files)[splits[[i]]])
  }))
  wtemp_indx = file.path(path, sim, paste0(sim, '_wtemp_index.tsv'))
  write.table(wtemp_zip_index, wtemp_indx, sep='\t', row.names=FALSE)
  
  
  for(i in 1:length(splits)){
    zip(zipfile=wtemp_zips[i], files=all_wtr_files[splits[[i]]])
  }
  #delete raw text files to save space
  unlink(all_wtr_files)
  
  # #upload files to SB when done
  # authenticate_sb(user, pass)
  # itm_title = paste0('Simulated lake temperatures for ', sim, ' future projections')
  # sim_itm = item_create(parent_id=sb_itm_root, title=itm_title)
  # 
  # item_append_files(sim_itm, files=c(core_path, cfg_path, hansen_path, wtemp_zips, wtemp_indx))
  print(c(core_path, cfg_path, hansen_path, wtemp_zips, wtemp_indx))
  return(c(core_path, cfg_path, hansen_path, wtemp_zips, wtemp_indx))
}

