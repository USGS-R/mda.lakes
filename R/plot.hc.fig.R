# plot figure 8 for Jamon's canopy manuscript
source('GLM.functions.R')
get.wSt <- function(lkeArea,hs){
  minWstr  <-  0.0001
  
  Xt	<-	50*hs
  D	<-	2*sqrt(lkeArea/pi)
  D[D<Xt] = Xt[D<Xt]
  wind.shelter	<-	2/pi*acos(Xt/D)-(2*Xt/(pi*D^2))*sqrt(D^2-Xt^2)
  wind.shelter[wind.shelter==0]=minWstr
  return(wind.shelter)
}

plot.hc.fig <- function(){
  file.nm <- "Fig8_multi-h_s_comparison.csv"
  dat <- read.table(paste('../supporting files/',file.nm,sep=''),
             sep=',',header=TRUE,colClasses=c('character',rep('numeric',5)))
  WBICs = dat[,1]
  area = dat[,2]
  ASTER_hs = dat$ASTER_h_s
  ASTER_hs[ASTER_hs==-99999] <- NA
  SRTM_hs = dat$SRTM_h_s
  SRTM_hs[SRTM_hs==-99999] <- NA
  SIMARD_hc = dat$SIMARD_h_c
  SIMARD_hc[SIMARD_hc==-99999] <- NA
  GLIHT_hs= dat$GLIHT_h_s
  GLIHT_hs[GLIHT_hs==-99999] <- NA
  boxplot(ASTER_hs)
  return(dat)
}
dat = plot.hc.fig()