# plot figure 8 for Jamon's canopy manuscript
source('GLM.functions.R')
get.wSt <- function(lkeArea,hs=NULL,method='Markfort'){
  minWstr  <-  0.0001
  minhs <- 0.1
  
  if (method=='Markfort'){
    nan.i = is.na(hs) | hs==0
    hs[nan.i] = 9999
    #hs[hs<minhs]=minhs
    Xt  <-	50*hs
    D	<-	2*sqrt(lkeArea/pi)
    D[D<Xt] = Xt[D<Xt]
    wind.shelter	<-	2/pi*acos(Xt/D)-(2*Xt/(pi*D^2))*sqrt(D^2-Xt^2)
    wind.shelter[nan.i]=NA
    
  } else if (method=='Hondzo'){
    lkeArea.km2 = lkeArea*1.0e-6 # to km2
    wind.shelter= 1.0 - exp(-0.3*lkeArea.km2)
  }
  
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
  plot.frame = data.frame('ASTER'=get.wSt(area,ASTER_hs),
                          'Hondzo'=get.wSt(area,method="Hondzo"),
                          'SRTM'=get.wSt(area,SRTM_hs),
                          'SIMARD'=get.wSt(area,SIMARD_hc),
                          'GLIHT'=get.wSt(area,GLIHT_hs))
  #boxplot(plot.frame)
  srt.area = sort(area)
  plot(srt.area*1.0e-6,get.wSt(srt.area,method="Hondzo"),
       log='x',col='red',cex=0.001,
       ylab="W_st",xlab="Lake size (km2)",lty=1)
  poly.x = seq(log(min(area)),log(max(area)),length.out=1000)
  polygon(x=c(exp(poly.x)*1.0e-6,rev(exp(poly.x)*1.0e-6)), y=c(get.wSt(exp(poly.x),0.2),rev(get.wSt(exp(poly.x),1.4))),
          col = rgb(228,229,99, max = 255) ,border=NA)
  polygon(x=c(exp(poly.x)*1.0e-6,rev(exp(poly.x)*1.0e-6)), y=c(get.wSt(exp(poly.x),8),rev(get.wSt(exp(poly.x),15))),
          col = rgb(83,145,61, max = 255),border=NA)
  lines(srt.area*1.0e-6,get.wSt(srt.area,method="Hondzo"),
       col='red',cex=0.5,lwd=2.5)
  points(area*1.0e-6,get.wSt(area,ASTER_hs),pch=5,cex=0.1,col='grey30')
  points(c(1.71,3.3,1.23,1.16,0.85,10,7.7,0.07,0.35),c(0.4,.6,.5,.5,.1,.9,.8,.01,.2),pch=24,cex=1,col='firebrick',lwd=3,bg='grey80')
  
  return(dat)
}
dat = plot.hc.fig()