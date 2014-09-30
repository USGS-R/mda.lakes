## Air Temp Trend

source('sens.confint.mod.R')

all.slopes.perms = function(times, data){
	
	if(length(data)==2){
		perm.i = matrix(c(1,2))
		#return((data[1]-data[2])/(times[1]-times[2]))
	}else{
		perm.i = combn(1:length(data),2)
	}
	
	
	#perm1 = data[perm.i[1,],]
	#perm2 = data[perm.i[2,],]
	starts = apply(matrix(times[c(perm.i[1,], perm.i[2,])], ncol=2), 1, min)
	ends   = apply(matrix(times[c(perm.i[1,], perm.i[2,])], ncol=2), 1, max)
	dts    = abs(times[perm.i[1,]] - times[perm.i[2,]])
	
	slopes = (data[perm.i[1,]]-data[perm.i[2,]])/(times[perm.i[1,]] - times[perm.i[2,]])
	
	output = data.frame(slopes, start=starts, end=ends, dt=diff(range(times)), n.obs=length(times))
	return(output)
}


airt = data.frame(fread('../supporting files/wisco.avg.airT.csv')[Year >= 1990,])


output = data.frame()
for(i in 2:13){
	tmp = all.slopes.perms(airt$Year, airt[,i])
	tmp$month = i-1
	output = rbind(output, tmp)
}

median(output$slopes)

boxplot(slopes~month, output, ylim=c(-1,1))
abline(0, 0)

