plot.fig8.GCB	<-	function(years){
	lW = 3
	cex.box = 0.8
	cex.ttl = 0.8
	tck	<-	0.02
	plt.rng.x	= c(75,143)
	seq.range = seq(70,365,1)
	plot_colors <- c("black", "firebrick", "grey80","grey40")
	
	fig.w	<-	3.14961
	v.spc	<-	0.075 # inches of vertical space to separate panels (top only)
	h.spc	<-	0.00 # inches of vertical space to separate panels (right only)
	l.mar	<-	0.05
	r.mar	<-	v.spc
	t.mar	<-	0.00
	left.spc	<-	0.25
	pan.size	<-	(fig.w-2*(h.spc+left.spc)-r.mar-l.mar)/2
	divs	<-	5
	
	bot.buffer	<-	pan.size/divs	# inches at base to allow 
	fig.h	<-	(pan.size+v.spc)*3+bot.buffer+t.mar
	print(fig.h)
	
	png(filename = "../Figure_08.png",
	    width = fig.w, height = fig.h, units = "in", res=300)

	panels = NULL
	for (j in 1:3){	
		for (i in 1:divs){
			panels	<-	rbind(panels,c(j,j+3))
		}
	}
	panels	<-	rbind(panels,c(j,j+3)) # last one!
	plot(c(NA,1),c(0,1), type="l", col=NA, 
		axes=F, xlab=NA,
		ylim=c(-10,25), xlim=plt.rng.x,
		ylab="Air temperature (°C)",
		xaxs="i", yaxs="i",cex.lab=cex.ttl)	# faux plot axes=F,
	
	
	layout(panels)
	
	par(mai=c(.0,left.spc, v.spc, h.spc),mgp=c(.9,.14,0),omi=c(0,l.mar,t.mar,r.mar))
	
	# plot 1 & 4 are air temperatures
	plot.air(years[1],col=plot_colors[2],plt.rng.x,cex.ttl,cex.box,tck,label='(a)')
 	
	plot.exceed(years[1],col=plot_colors,plt.rng.x,cex.ttl,cex.box,tck,label='(c)')
	par(mai=c(fig.h/((divs*3)+1),left.spc, v.spc, h.spc))
	
	plot.strat(years[1],col=plot_colors,plt.rng.x,cex.ttl,cex.box,tck,label='(e)')
	par(mai=c(.0,left.spc, v.spc, h.spc))
	
	plot.air(years[2],col=plot_colors[2],plt.rng.x,cex.ttl,cex.box,tck,label='(b)')
 	
	
	plot.exceed(years[2],col=plot_colors,plt.rng.x,cex.ttl,cex.box,tck,label='(d)')
	par(mai=c(fig.h/((divs*3)+1),left.spc, v.spc, h.spc))
	plot.strat(years[2],col=plot_colors,plt.rng.x,cex.ttl,cex.box,tck,label='(f)')

	dev.off()
}

plot.air	<-	function(year,col,plt.rng.x,cex.ttl,cex.box,tck,label){
	plot(c(NA,1),c(0,1), type="l", col=NA, 
		axes=F, xlab=NA,
		ylim=c(-15,26), xlim=plt.rng.x,
		ylab="Air temperature (°C)",
		xaxs="i", yaxs="i",cex.lab=cex.ttl)
	axis(1,at=seq(0, 200, 20),las=1, cex.axis=cex.box, tck=tck,labels=NA)
	axis(3,at=seq(0, 200, 20),las=1, cex.axis=cex.box, tck=tck,labels=NA)
	axis(2,at=seq(-100, 50, 10),las=1, cex.axis=cex.box, tck=tck)
	axis(4,at=seq(-100, 50, 10),las=1, cex.axis=cex.box, tck=tck,labels=NA)
	air.temps	<-	get.air.temp(year)
	lines(air.temps$time,air.temps$temperature,type="l", lwd=2,
		col=col)
	label.loc	<-	get.text.location(par())
	text(label.loc[1],label.loc[2],label)
	
}

plot.exceed	<-	function(year,col,plt.rng.x,cex.ttl,cex.box,tck,label){
	plot(c(NA,1),c(0,1), type="l", col=col[1], 
		axes=F, xlab=NA,
		ylim=c(0,100), xlim=plt.rng.x,
		ylab="Lakes warmer than 8.9°C (%)",
		xaxs="i", yaxs="i",cex.lab=cex.ttl)
	axis(1,at=seq(0, 200, 20),las=1, cex.axis=cex.box, tck=tck,labels=NA)
	axis(3,at=seq(0, 200, 20),las=1, cex.axis=cex.box, tck=tck,labels=NA)
	axis(2,at=seq(0, 100, 25),las=1, cex.axis=cex.box, tck=tck)
	axis(4,at=seq(0, 100, 25),las=1, cex.axis=cex.box, tck=tck,labels=NA)
	
	exceed	<-	get.hist.exceed(year)
	polygon(x=c(exceed$x,rev(exceed$x)), y=c(exceed$line.1,rev(exceed$line.5)),
		col = col[3],border=NA)
	polygon(x=c(exceed$x,rev(exceed$x)), y=c(exceed$line.2,rev(exceed$line.4)),
		col = col[4],border=NA)
	lines(exceed$x,exceed$line.3,type="l", lwd=2,
		col=col[1])
	label.loc	<-	get.text.location(par())
	text(label.loc[1],label.loc[2],label)
}

plot.strat	<-	function(year,col,plt.rng.x,cex.ttl,cex.box,tck,label){
	plot(c(NA,1),c(0,1), type="l", col=col[1], 
		axes=F, xlab="Day of year",
		ylim=c(0,100), xlim=plt.rng.x,
		ylab="Lakes stratified (%)",
		xaxs="i", yaxs="i",cex.lab=cex.ttl)		
	par(mgp=c(.9,.04,0))
	axis(1,at=seq(0, 200, 20),las=1, cex.axis=cex.box, tck=tck)
	par(mgp=c(.9,.14,0))
	axis(3,at=seq(0, 200, 20),las=1, cex.axis=cex.box, tck=tck,labels=NA)
	axis(2,at=seq(0, 100, 25),las=1, cex.axis=cex.box, tck=tck)
	axis(4,at=seq(0, 100, 25),las=1, cex.axis=cex.box, tck=tck,labels=NA)
	
	strat	<-	get.strat.count(year)
	lines(strat$x,strat$line.1,type="l", lwd=2,
		col=col[1])
	label.loc	<-	get.text.location(par())
	text(label.loc[1],label.loc[2],label)
}

get.air.temp	<-	function(year){
	air.file	<-	"../supporting files/obs_Tair_WI_state.csv"
	# --- read in air temps for the year, plot on second axis ---

	air.temp	<-	read.delim(air.file,sep=',')

	air.dates	<-	as.POSIXct(air.temp$TIMESTEP,origin='UTC')
	air.temps	<-	air.temp$MEAN.C.
	# truncate according to year...
	time.1	<-	paste(year,'-1-1',sep='')
	time.2	<-	paste(year,'-12-31 23:59',sep='')
	use.idx	<-	which(air.dates>=as.POSIXct(time.1,origin='UTC') & air.dates < as.POSIXct(time.2,origin='UTC'))
	air.dates	<-	as.numeric(strftime(air.dates[use.idx], format = "%j"))
	air.temps	<-	air.temps[use.idx]
	return(data.frame(time=air.dates,temperature=air.temps))
}

get.hist.exceed	<-	function(year,seq.range = seq(70,155,1)){
	threshold	<-	8.9
	DoY.file	<-	paste("../supporting files/exceed_",threshold,"C_errors_",year,'.tsv',sep='')
	DoY	<-	read.delim(DoY.file,sep='\t')

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

get.strat.onset	<-	function(year,seq.range = seq(70,365,1)){
	DoY.file	<-	paste("../supporting files/strat.onset",year,'.tsv',sep='')
	DoY	<-	read.delim(DoY.file,sep='\t')
	# warm median --
	dg = hist(DoY$strat.onset.DoY,seq.range,plot=FALSE)
	x	<-	dg$breaks[-1]+diff(seq.range[1:2])*.5
	line.1	<-	cumsum(dg$counts/length(DoY$strat.onset.DoY))*100
	return(data.frame(x=x,line.1=line.1))
}

get.strat.count	<-	function(year){
	DoY.file	<-	paste("../supporting files/is.strat_",year,'.tsv',sep='')
	DoY	<-	read.delim(DoY.file,sep='\t')
	headers	<-	names(DoY)
	count.head 	<- tail(headers,1)
	num.lakes	<-	as.numeric(tail(strsplit(count.head,'.',fixed=T)[[1]],1))
	# warm median --
	x = DoY$DoY
	line.1	<-	DoY[,2]/num.lakes*100
	return(data.frame(x=x,line.1=line.1))
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
plot.fig8.GCB(years=c(1998,1996))
