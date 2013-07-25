data <- read.table('/Users/jread/Documents/WiLMA/supporting files/Validation/iceModel_onoff_all.tsv',header=TRUE)

source('GLM.functions.R')
# find all unique WBICs:
unWBIC 	<-	unique(data$WBIC)

# -- ice on --
on.int		<-	57.95749
on.zero_fl	<-	1.47585 
on.ang_fl	<-	-3.38068 
on.mn_dp5	<-	8.99045

# -- ice off --
#int:		-3.194e+02
#zero_sp:	1.821e+00
#ang_sp:	2.987e+00
#SA:		1.424e-02
#long:		-1.394e+00
#EL:		2.748e-02
#mxDepth:	5.608e-02

j = 1
useI	<-	data$WBIC==unWBIC[j]
tDat	<-	data[useI,]
print(length(tDat[,1]))

print(tDat)
for (j in 1:2){#length(unWBIC)){
	meanZ	<-	getZmean(unWBIC[j])
	mn_dp5	<-	meanZ^0.5
	useI	<-	data$WBIC==unWBIC[j]
	tDat	<-	data[useI,]
	for (i in 1:length(tDat[,1])){ # loop through years
		yrDat	<-	tDat[i,]
		print(mn_dp5)
		ice.on.DoY	<-	on.int+on.zero_fl*yrDat$zero_fl+on.ang_fl*yrDat$ang_fl+on.mn_dp5*mn_dp5
		print(ice.on.DoY)
	}
}

