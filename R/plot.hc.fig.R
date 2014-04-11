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

	fig.w	<-	3.14961
	  fig.h <- 2.5
	  l.mar	<-	0.45
	  r.mar	<-	0.05#v.spc
	  t.mar	<-	0.05
	  b.mar	<-	0.4
	  cex.box = .8
	  cex.ttl = 0.8
	  tck	<-	-0.01
	  tick.x <- seq(40,160,10)
	  tick.y <- seq(2,30,2)
	  par.mgp  <-	data.frame(x=c(1.1,.05,0),y=c(1.1,.2,0))
	  y.lim <- c(-.01,1.0)
	  x.lim <- c(1e-4,1.5e2)
	tick.x	<-	c(1e-4,1e-3,1e-2,1e-1,1e0,1e1,1e2,1e3)
	tick.x.lab <- c(as.expression(bquote(10^ .(-4))),as.expression(bquote(10^ .(-3))),as.expression(bquote(10^ .(-2))),
		as.expression(bquote(10^ .(-1))), as.expression(bquote(10^ .(0))) ,
		as.expression(bquote(10^ .(1))) ,as.expression(bquote(10^ .(2))),as.expression(bquote(10^ .(3))))
		
	tick.y <- seq(-.2,1,.2)

	svg("../Figure_Hondzo.svg",width = fig.w, height = fig.h, family="Helvetica")
#	  png(filename = "../Figure_Hondzo.png",
#	      width = fig.w, height = fig.h, units = "in", res=300,family="Helvetica")
	suppressWarnings(par(mgp=par.mgp$x,omi=c(0,0,0,0),mai=c(b.mar,l.mar, t.mar, r.mar)))
  	srt.area = sort(area)
	axisx.label = expression(paste("Lake size (km"^"2",")"))#parse(text = "Lake size (km^2*N)")
	axisy.label = expression(paste("W"["st"]))
  	plot(srt.area*1.0e-6,get.wSt(srt.area,method="Hondzo"),
    	log='x',col='red',cex=0.001,
		xaxs='i',yaxs='i',axes=F,
		ylim=y.lim , xlim=x.lim ,
		ylab=axisy.label,xlab=axisx.label,lty=1,cex.lab=1)
	
  	poly.x = seq(log(min(area)),log(max(area)),length.out=1000)
  	polygon(x=c(exp(poly.x)*1.0e-6,rev(exp(poly.x)*1.0e-6)), y=c(get.wSt(exp(poly.x),0.2),rev(get.wSt(exp(poly.x),1.4))),
       	col = rgb(228,229,99, max = 255) ,border=NA)
  	polygon(x=c(exp(poly.x)*1.0e-6,rev(exp(poly.x)*1.0e-6)), y=c(get.wSt(exp(poly.x),8),rev(get.wSt(exp(poly.x),15))),
       	col = rgb(83,145,61, max = 255),border=NA)
  	lines(srt.area*1.0e-6,get.wSt(srt.area,method="Hondzo"),
       	col='red',cex=0.5,lwd=2.5)

	axis(1,las=1, at=tick.x,cex.axis=cex.box, tck=tck, labels=tick.x.lab)
	axis(3,at=c(1e-10,1e10),las=1, cex.axis=cex.box)
	suppressWarnings(par(mgp=par.mgp$y))
	axis(2,las=1, at=tick.y,cex.axis=cex.box, tck=tck)
	axis(4,at=c(-1000,1000),las=1, cex.axis=cex.box)
  points(c(1.71,3.3,1.23,1.16,0.85,10,7.7,0.07,0.35),c(0.4,.6,.5,.5,.1,.9,.8,.01,.2),
		pch=24,cex=1.0,col='firebrick',lwd=1,bg='grey80')
  return(dat)
	dev.off()
}
dat = plot.hc.fig()