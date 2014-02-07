# plot figure 8 for Jamon's canopy manuscript

plot.hc.fig <- function(){
  file.nm <- "Fig8_multi-h_s_comparison.csv"
  dat <- read.table(paste('../supporting files/',file.nm,sep=''),
             sep=',',header=TRUE,colClasses=c('character',rep('numeric',5)))
  
  WBICs = dat[,1]
  area = dat[,2]
  ASTER_hs = dat$ASTER_h_s
  SRTM_hs = dat$SRTM_h_s
  SIMARD_hc = dat$SIMARD_h_c
  GLIHT_hs= dat$GLIHT_h_s
  
  return(dat)
}
dat = plot.hc.fig()