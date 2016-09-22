#' @title Upload and archive model to ScienceBase
#' 
#' @description 
#' Creates relevant child items a
#' 
#' @param path Path to folder containing sim files
#' @param sb_root SB root item ID to create new items under
#' @param sbuser SB user name
#' @param sbpass SB password
#' 
#' @import sbtools
#' 
#' @export
sb_archive_model = function(path, sb_root, sbuser, sbpass){
  
  sim = basename(path)
  allfiles = dir(path)
  ndone = 0
  pb = txtProgressBar(min = 0, max = length(allfiles))
  
  authenticate_sb(sbuser, sbpass)
  itm_title = paste0('Simulated lake temp metrics for ', sim)
  sim_itm = item_create(parent_id=sb_root, title=itm_title)
  
  ## core metrics create/upload
  core_met = item_create(parent_id=sim_itm, title=paste0(sim, ':Core thermal metrics'))
  files = Sys.glob(file.path(path, '*_core_metrics.tsv'))
  item_append_files(core_met, files)
  setTxtProgressBar(pb, (ndone <- ndone+1))
  
  ## fish habitat
  fish_hab = item_create(parent_id=sim_itm, title=paste0(sim, ':Fish habitat metrics'))
  files = Sys.glob(file.path(path, '*_fish_hab.tsv'))
  item_append_files(fish_hab, files)
  setTxtProgressBar(pb, (ndone <- ndone+1))
  
  ## model configuration
  mod_conf = item_create(parent_id=sim_itm, title=paste0(sim, ':Model configuration input'))
  files = Sys.glob(file.path(path, '*_model_config.json'))
  item_append_files(mod_conf, files)
  setTxtProgressBar(pb, (ndone <- ndone+1))
  
  
  cat('starting wtr files upload...')
  ## water temp files
  wtr_raw = item_create(parent_id=sim_itm, title=paste0(sim, ':Raw water temperature'))
  files = Sys.glob(file.path(path, '*_wtemp_*'))
  for(i in 1:length(files)){
    item_append_files(wtr_raw, files=files[i])
    setTxtProgressBar(pb, (ndone <- ndone+1))
  }
  
  close(pb)
}
