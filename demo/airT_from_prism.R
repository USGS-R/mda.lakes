library(geoknife)

sb_wfs <- 'https://www.sciencebase.gov/catalogMaps/mapping/ows/54668b7ce4b04d4b7dbda52d'
stencil <- webgeom(url = sb_wfs, geom = "sb:managed_wiscoNoZ_wgs84", attribute = "WBDY_WBIC")
  
var <- 'tmx'
fabric <- webdata('prism',times = c('1979-01-01', NA), variables = var)
job <- geoknife(stencil, fabric, wait=T, DELIMITER='TAB')
download(job, destination = paste0('data/prism_',var,'.tsv'), overwrite=T)
var <- 'tmn'
job <- geoknife(stencil, fabric, wait=T, DELIMITER='TAB')
download(job, destination = paste0('data/prism_',var,'.tsv'), overwrite=T)
