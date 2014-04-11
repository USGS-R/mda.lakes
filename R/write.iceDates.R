data <- read.table('../supporting files/Validation/iceModel_onoff_all.tsv',header=TRUE)

source('Libraries/GLM.functions.R')
# find all unique WBICs:
unWBIC 	<-	unique(data$WBIC)

# -- ice on --
on.int		<-	57.95749
on.zero_fl	<-	1.47585 
on.ang_fl	<-	-3.38068 
on.mn_dp5	<-	8.99045

# -- ice off --
off.int		<-	-3.194e+02
off.zero_sp	<-	1.821e+00
off.ang_sp	<-	2.987e+00
off.SA		<-	1.424e-02
off.long	<-	-1.394e+00
off.EL		<-	2.748e-02
off.mxDepth	<-	5.608e-02


for (j in 1:length(unWBIC)){
	meanZ	<-	getZmean(unWBIC[j])
	mn_dp5	<-	meanZ^0.5
	useI	<-	data$WBIC==unWBIC[j]
	tDat	<-	data[useI,]
	for (i in 1:length(tDat[,1])){ # loop through years
		yrDat	<-	tDat[i,]
		ice.on.DoY	<-	on.int+on.zero_fl*yrDat$zero_fl+on.ang_fl*yrDat$ang_fl+on.mn_dp5*mn_dp5
		ice.off.DoY	<-	off.int+off.zero_sp*yrDat$zero_sp+off.ang_sp*yrDat$ang_sp+
			off.SA*yrDat$SA+off.long*yrDat$long+off.EL*yrDat$EL+off.mxDepth*yrDat$mxDepth
		
		if (j==1 && i ==1){
			write.out	<-	list('WBIC'=unWBIC[j],'YYYY'=yrDat$YYYY,
			'ice.off.DoY'=ice.off.DoY,'ice.on.DoY'=ice.on.DoY)
		} else {
			write.out	<-	rbind(write.out,list('WBIC'=unWBIC[j],'YYYY'=yrDat$YYYY,
			'ice.off.DoY'=ice.off.DoY,'ice.on.DoY'=ice.on.DoY))
		}
	}

}

# write to file
output = "ChainedRun/iceOnOffDoY.tsv"
write.table(write.out,file=output,col.names=TRUE, quote=FALSE, row.names=FALSE, sep="\t")
