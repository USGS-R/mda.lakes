plot.fig8.GCB	<-	function(years){
	lW = 3
	cex.box = 1
	cex.ttl = 1
	tck	<-	0.02
	font.size = 13
	plt.rng.x	= c(55,145)
	
	plot_colors <- c("black", "black", "grey80","grey40")
	
	h_w.ratio	<-	1.1 # height to width ratio
	fig.w	<-	2
  fig.h <- 2.
	v.spc	<-	0.05 # inches of vertical space to separate panels (top only)
	h.spc	<-	0.05 # inches of vertical space to separate panels (right only)
	l.mar	<-	0.02
	r.mar	<-	0.01#v.spc
	t.mar	<-	0.00
  b.mar <- 0.4
	left.spc	<-	0.45

	par.mgp <<- data.frame(x=c(1.35,.08,0),y=c(1.25,.05,0),y.s=c(1.2,.05,0))
	print(fig.h)
	
  
	png(filename = "/Users/jread/Desktop/Science Projects/NE CSC/Full proposal/Figures/Figure_expected.png",
	    width = fig.w, height = fig.h, units = "in", res=250)


  
	plot(c(NA,1),c(0,1), type="l", col=NA, 
		axes=F, xlab=NA,
		ylim=c(-10,25), xlim=plt.rng.x,
		ylab="Lakes warmer than 8.9°C (%)",
		xaxs="i", yaxs="i",cex.lab=cex.ttl)	# faux plot axes=F,
	
	
	
	par(mai=c(b.mar,left.spc, v.spc, h.spc),mgp=par.mgp$x,omi=c(0,l.mar,t.mar,r.mar),ps=font.size)
	
	plot.exceed(years[1:2],years[3],col=plot_colors,plt.rng.x,cex.ttl,cex.box,tck)
	

	dev.off()
}


plot.exceed	<-	function(years_range,year_future, col,plt.rng.x,cex.ttl,cex.box,tck){
  par(mgp=par.mgp$y)
	plot(c(NA,1),c(0,1), type="l", col=col[1], 
		axes=F, xlab="",
		ylim=c(0,100), xlim=plt.rng.x,
		ylab="Lakes warmer than 8.9°C (%) ",
		xaxs="i", yaxs="i",cex.lab=cex.ttl)

  par(mgp=par.mgp$y.s)
  title(xlab="Day of year",cex.lab=cex.ttl)
  par(mgp=par.mgp$x)
	exceed_1	<-	get.hist.exceed(years_range[1])
  med_x1 <- get_median_exceed(exceed_1$line.3)
  exceed_2  <-	get.hist.exceed(years_range[2])
  med_x2 <- get_median_exceed(exceed_2$line.3)
  polygon(c(exceed_1$x,rev(exceed_2$x)),c(exceed_1$line.3,rev(exceed_2$line.3)), 
          col='grey80',   border = 'grey60')
  
  exceed_ftr  <-	get.hist.exceed(year_future)
  med_ftr <- get_median_exceed(exceed_ftr$line.3)
	lines(exceed_ftr$x,exceed_ftr$line.3,type="l", lwd=3,
		col='red')
  
  #lines(c(0,exceed$x[med_x]),c(50,50), lty=3, col = 'black' )
  #lines(c(exceed$x[med_x], exceed$x[med_x]),c(0,50), lty=3, col = 'black' )
  
  par(mgp=par.mgp$y.s)
	axis(1,at=seq(0, 200, 20),las=1, cex.axis=cex.box, tck=tck)
	axis(3,at=seq(0, 200, 20),las=1, cex.axis=cex.box, tck=tck,labels=NA)
	axis(2,at=seq(0, 100, 25),las=1, cex.axis=cex.box, tck=tck)
	axis(4,at=seq(0, 100, 25),las=1, cex.axis=cex.box, tck=tck,labels=NA)
}

get_median_exceed <- function(line_med){
  
  idx <- head(which(line_med>=50),1)
  return(idx)
}

get.hist.exceed	<-	function(year,seq.range =seq(0,365,1)){
	threshold	<-	8.9
  
  
	DoY.file	<-	paste("../../supporting files/exceed_",threshold,"C_errors_",year,'.tsv',sep='')
	DoY	<-	read.delim(DoY.file,sep='\t')

	if (year > 2000){
    cat('adding to dist for future...\n')
	  DoY$exceed8.9.C.50.. = (DoY$exceed8.9.C.50..+5)*0.75
	}
	# 95 percentiles
	h.low = hist(DoY$exceed8.9.C.2.5..,seq.range,plot=FALSE)
	x	<-	h.low$breaks[-1]+diff(seq.range[1:2])*.5
	line.1 = cumsum(h.low$counts/sum(h.low$counts))*100

	h.high = hist(DoY$exceed8.9.C.97.5..,seq.range,plot=FALSE)
	line.5 = cumsum(h.high$counts/sum(h.low$counts))*100

	# quartiles
	h.low = hist(DoY$exceed8.9.C.25..,seq.range,plot=FALSE)
	line.2 = cumsum(h.low$counts/sum(h.low$counts))*100

	h.high = hist(DoY$exceed8.9.C.75..,seq.range,plot=FALSE)
	line.4	<-	cumsum(h.high$counts/sum(h.low$counts))*100

	# warm median --
	dg = hist(DoY$exceed8.9.C.50..,seq.range,plot=FALSE)
	line.3	<-	cumsum(dg$counts/sum(dg$counts))*100
	
	return(data.frame(x=x,line.1=line.1,line.2=line.2,line.3=line.3,line.4=line.4,line.5=line.5))
}

get.text.location	<-	function(par,perc=10){
	x.lim	<-	par$usr[1:2] # limits
	y.lim	<-	par$usr[3:4]
	# upper right hand
	y.range	<-	y.lim[2]-y.lim[1]
	x.range <-	x.lim[2]-x.lim[1]
	
	y	<-	y.lim[2]-y.range*perc/100
	x	<-	x.lim[1]+x.range*perc/100
	return(c(x,y))
	
}
plot.fig8.GCB(years=c(1996, 1998, 2009))
