plot.sensitivity	<-	function(){
	source('Libraries/GLM.functions.R')
	param	<-	'hc'
	
	small.thresh	<-	1000000 # 100 ha
	large.thresh	<-	10000000	# 10000 ha
#	png(filename = "../Figure_sensitivity.png",
#	    width = 3, height = 2, units = "in", res=300)
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
	plot(sens.x,diffs$small,col='blue',type='l',xlim=c(.4,1.6),ylim=c(-4,4))
	points(sens.x,diffs$medium,col='green',type='l')
	points(sens.x,diffs$large,col='red',type='l')

}

plot.sensitivity()