plot.fig7.GCB	<-	function(years){

	cex.box = 0.8
	cex.ttl = 0.8
	tck	<-	0.02
	plt.rng.x	<-	c(.25,1.75)
	plt.rng.y	<-	data.frame("Kw"=c(-6,6),"RT"=c(-1.2,1.2),"hc"=c(-1.2,1.2))
	tick.x	<-	c(-20,0.5,1,1.5,50)
	tick.y	<-	data.frame("Kw"=c(-10,-5,NA,0,NA,5,10),"RT"=c(-10,-1,NA,0,NA,1,10),"hc"=c(-10,NA,-1,0,NA,1,10))
	tick.x.lab	<-	c(NA,"-50%","0","+50%",NA)
	plot_colors <<- c("black", 'grey65', "grey80","grey40")
	lab.perc	<<-	12 # label (e.g., (a) spacing percent from corner)
	par.mgp	<<-	data.frame(x=c(.65,.1,0),y=c(-.3,-.06,0))
	small.thresh	<<-	1000000 # 100 ha
	large.thresh	<<-	1e20	# effectively no upper limit
	plot.order	<<-	c("medium","small","large")
	line.wd	<<-	1.1	# plot line width
	
	fig.w	<-	3.14961
	v.spc	<-	0.05 # inches of vertical space to separate panels (top only)
	h.spc	<-	0.05 # inches of horizontal space to separate panels (right only)
	l.mar	<-	0.0
	r.mar	<-	0#v.spc
	t.mar	<-	0.01
	b.mar	<-	0.25
	left.spc	<-	0.25 #0.1
	pan.size	<-	(fig.w-3*(h.spc+left.spc)-r.mar-l.mar)/3
	fig.h	<-	pan.size+v.spc+b.mar+t.mar
	print(fig.h)
	print(fig.w)
	png(filename = "../Figure_07.png",
	    width = fig.w, height = fig.h, units = "in", res=450)

	panels = matrix(c(1,2,3),1,3)
	
	
	layout(panels)
	
	par(mai=c(b.mar,left.spc, v.spc, h.spc),mgp=par.mgp$x,omi=c(0,l.mar,t.mar,r.mar))

	plot.kw(year,col,plt.rng.x,plt.rng.y,cex.ttl,cex.box,tick.x,tick.y,label,tck,tick.x.lab)
	plot.RT(year,col,plt.rng.x,plt.rng.y,cex.ttl,cex.box,tick.x,tick.y,label,tck,tick.x.lab)
	plot.hc(year,col,plt.rng.x,plt.rng.y,cex.ttl,cex.box,tick.x,tick.y,label,tck,tick.x.lab)
	
	

	
	dev.off()
}

plot.kw	<-	function(year,col,plt.rng.x,plt.rng.y,cex.ttl,cex.box,tick.x,tick.y,label,tck,tick.x.lab){
	source('GLM.functions.R')
	param	<-	'Kw'
	write.param <- expression(paste("Change in K"["d"]," (%)"))
	#par(mgp=c(.9,.06,0))
	plot(c(0,1),c(0,1), type="l", col=NA, 
		axes=F,
		ylim=plt.rng.y$Kw, xlim=plt.rng.x,
		ylab="Temp. difference (°C)",
		xlab=write.param,
		xaxs="i", yaxs="i",cex.lab=cex.ttl)
	
	par(mgp=par.mgp$y)
	axis(1,las=1, at=tick.x,cex.axis=cex.box, tck=tck,labels=tick.x.lab)
	axis(3,at=tick.x,las=1, cex.axis=cex.box, tck=tck,labels=NA)
	par(mgp=par.mgp$x)
	axis(2,las=1, at=tick.y$Kw,cex.axis=cex.box, tck=tck)
	axis(4,at=tick.y$Kw,las=1, cex.axis=cex.box, tck=tck,labels=NA)	
	


	sens.file	<-	paste("../supporting files/sensitivity_relative_",param,'.tsv',sep='')
	sens.table	<-	read.delim(sens.file,sep='\t')

	
	WBICs	<-	as.character(sens.table$WBICs)
	
	sens.vals	<-	sens.table[,-1]

	sizes	<-	vector(length=length(WBICs))
	# do size categories too!
	for (j in 1:length(WBICs)){
		sizes[j]	<-	getArea(WBICs[j])
	}
	
	num.sims	<-	length(names(sens.vals))
	sens.x	<-	vector(length=num.sims)
	head.length 	<-	nchar(paste(param,"_relative_",sep=''))
	paste(param,"_relative_",sep='')
	for (k in 1:num.sims){
		sens.x[k] <- as.numeric(substring(names(sens.vals)[k],head.length+1))
	}

	comp.idx	<-	which(sens.x==1)
	
	diffs	<-	data.frame("small"=vector(length=length(sens.x)),
		"medium"=vector(length=length(sens.x)),
		"large"=vector(length=length(sens.x)))
	
	small.i	<-	(sizes<=small.thresh)
	med.i	<-	(sizes>small.thresh & sizes<large.thresh)
	large.i	<-	(sizes>=large.thresh)
	for (k in 1:length(sens.x)){
		sim.norm	<-	sens.vals[,comp.idx]
		sim.comp	<-	sens.vals[,k]
		
		diffs$small[k]	<-	median(sim.comp[small.i]-sim.norm[small.i],na.rm=TRUE)
		diffs$medium[k]	<-	median(sim.comp[med.i]-sim.norm[med.i],na.rm=TRUE)
		diffs$large[k]	<-	median(sim.comp[large.i]-sim.norm[large.i],na.rm=TRUE)
	}
	lines(sens.x,diffs[[plot.order[1]]],col=plot_colors[1],type='l',lwd=line.wd)
	lines(sens.x,diffs[[plot.order[2]]],col=plot_colors[2],type='l',lwd=line.wd)

	label.loc	<-	get.text.location(par(),perc=lab.perc)
	text(label.loc[1],label.loc[2],label='(a)')
}

plot.RT	<-	function(year,col,plt.rng.x,plt.rng.y,cex.ttl,cex.box,tick.x,tick.y,label,tck,tick.x.lab){
	source('GLM.functions.R')
	param	<-	'RT'
	#par(mgp=c(.9,.06,0))
	plot(c(0,1),c(0,1), type="l", col=NA, 
		axes=F,
		ylim=plt.rng.y$RT, xlim=plt.rng.x,
		ylab="Temp. difference (°C)",
		xlab=paste("Change in ",param," (%)",sep=''),
		xaxs="i", yaxs="i",cex.lab=cex.ttl)
	
	par(mgp=par.mgp$y)
	axis(1,las=1, at=tick.x,cex.axis=cex.box, tck=tck,labels=tick.x.lab)
	axis(3,at=tick.x,las=1, cex.axis=cex.box, tck=tck,labels=NA)
	par(mgp=par.mgp$x)
	axis(2,las=1, at=tick.y$RT,cex.axis=cex.box, tck=tck)
	axis(4,at=tick.y$Kw,las=1, cex.axis=cex.box, tck=tck,labels=NA)	

	sens.file	<-	paste("../supporting files/sensitivity_relative_",param,'.ave.tsv',sep='')
	sens.table	<-	read.delim(sens.file,sep='\t')

	
	WBICs	<-	as.character(sens.table$WBICs)
	
	sens.vals	<-	sens.table[,-1]

	sizes	<-	vector(length=length(WBICs))
	# do size categories too!
	for (j in 1:length(WBICs)){
		sizes[j]	<-	getArea(WBICs[j])
	}
	
	num.sims	<-	length(names(sens.vals))
	sens.x	<-	vector(length=num.sims)
	head.length 	<-	nchar(paste(param,"_relative_",sep=''))
	paste(param,"_relative_",sep='')
	for (k in 1:num.sims){
		sens.x[k] <- as.numeric(substring(names(sens.vals)[k],head.length+1))
		
	}
	# -----
	sens.x = rev(sens.x) # because RT is the reverse of param increase (flow is being increased, RT goes down!!)
	comp.idx	<-	which(sens.x==1)
	
	diffs	<-	data.frame("small"=vector(length=length(sens.x)),
		"medium"=vector(length=length(sens.x)),
		"large"=vector(length=length(sens.x)))
	
	small.i	<-	(sizes<=small.thresh)
	med.i	<-	(sizes>small.thresh & sizes<large.thresh)
	large.i	<-	(sizes>=large.thresh)
	for (k in 1:length(sens.x)){
		sim.norm	<-	sens.vals[,comp.idx]
		sim.comp	<-	sens.vals[,k]
		
		diffs$small[k]	<-	median(sim.comp[small.i]-sim.norm[small.i],na.rm=TRUE)
		diffs$medium[k]	<-	median(sim.comp[med.i]-sim.norm[med.i],na.rm=TRUE)
		diffs$large[k]	<-	median(sim.comp[large.i]-sim.norm[large.i],na.rm=TRUE)
	}
	lines(sens.x,diffs[[plot.order[1]]],col=plot_colors[1],type='l',lwd=line.wd)
	lines(sens.x,diffs[[plot.order[2]]],col=plot_colors[2],type='l',lwd=line.wd)
	
	# -- other file
	sens.file	<-	paste("../supporting files/sensitivity_relative_",param,'.air.tsv',sep='')
	sens.table	<-	read.delim(sens.file,sep='\t')
	sens.vals	<-	sens.table[,-1]
	for (k in 1:length(sens.x)){
		sim.norm	<-	sens.vals[,comp.idx]
		sim.comp	<-	sens.vals[,k]
		
		diffs$small[k]	<-	median(sim.comp[small.i]-sim.norm[small.i],na.rm=TRUE)
		diffs$medium[k]	<-	median(sim.comp[med.i]-sim.norm[med.i],na.rm=TRUE)
		diffs$large[k]	<-	median(sim.comp[large.i]-sim.norm[large.i],na.rm=TRUE)
	}
	lines(sens.x,diffs[[plot.order[1]]],col=plot_colors[1],type='l',lwd=line.wd,lty=3)
	lines(sens.x,diffs[[plot.order[2]]],col=plot_colors[2],type='l',lwd=line.wd,lty=3)
	
	label.loc	<-	get.text.location(par(),perc=lab.perc)
	text(label.loc[1],label.loc[2],label='(b)')
}
plot.hc	<-	function(year,col,plt.rng.x,plt.rng.y,cex.ttl,cex.box,tick.x,tick.y,label,tck,tick.x.lab){
	source('GLM.functions.R')
	param	<-	'hc'
	write.param <- expression(paste("Change in h"["s"]," (%)"))
	plot(c(0,1),c(0,1), type="l", col=NA, 
		axes=F,
		xlab=write.param,
		ylim=plt.rng.y$hc, xlim=plt.rng.x,
		ylab="Temp. difference (°C)",
		xaxs="i", yaxs="i",cex.lab=cex.ttl)

	par(mgp=par.mgp$y)
	axis(1,las=1, at=tick.x,cex.axis=cex.box, tck=tck,labels=tick.x.lab)
	axis(3,at=tick.x,las=1, cex.axis=cex.box, tck=tck,labels=NA)
	par(mgp=par.mgp$x)
	axis(2,las=1, at=tick.y$hc,cex.axis=cex.box, tck=tck)
	axis(4,at=tick.y$hc,las=1, cex.axis=cex.box, tck=tck,labels=NA)


	sens.file	<-	paste("../supporting files/sensitivity_relative_",param,'.tsv',sep='')
	sens.table	<-	read.delim(sens.file,sep='\t')

	
	WBICs	<-	as.character(sens.table$WBICs)
	
	sens.vals	<-	sens.table[,-1]

	sizes	<-	vector(length=length(WBICs))
	# do size categories too!
	for (j in 1:length(WBICs)){
		sizes[j]	<-	getArea(WBICs[j])
	}
	
	num.sims	<-	length(names(sens.vals))
	sens.x	<-	vector(length=num.sims)
	head.length 	<-	nchar(paste(param,"_relative_",sep=''))
	paste(param,"_relative_",sep='')
	for (k in 1:num.sims){
		sens.x[k] <- as.numeric(substring(names(sens.vals)[k],head.length+1))
	}

	comp.idx	<-	which(sens.x==1)
	
	diffs	<-	data.frame("small"=vector(length=length(sens.x)),
		"medium"=vector(length=length(sens.x)),
		"large"=vector(length=length(sens.x)))
	
	small.i	<-	(sizes<=small.thresh)
	med.i	<-	(sizes>small.thresh & sizes<large.thresh)
	large.i	<-	(sizes>=large.thresh)
	for (k in 1:length(sens.x)){
		sim.norm	<-	sens.vals[,comp.idx]
		sim.comp	<-	sens.vals[,k]
		
		diffs$small[k]	<-	median(sim.comp[small.i]-sim.norm[small.i],na.rm=TRUE)
		diffs$medium[k]	<-	median(sim.comp[med.i]-sim.norm[med.i],na.rm=TRUE)
		diffs$large[k]	<-	median(sim.comp[large.i]-sim.norm[large.i],na.rm=TRUE)
	}
	lines(sens.x,diffs[[plot.order[1]]],col=plot_colors[1],type='l',lwd=line.wd)
	lines(sens.x,diffs[[plot.order[2]]],col=plot_colors[2],type='l',lwd=line.wd)
	label.loc	<-	get.text.location(par(),perc=lab.perc)
	text(label.loc[1],label.loc[2],label='(c)')
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
plot.fig7.GCB(years=c(1998,1996))
