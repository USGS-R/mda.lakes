sens.confint.mod = function(slopes, pval=0.9){
	
	slopes = sort(slopes)
	concor = sum(slopes > 0)
	discon = sum(slopes < 0)
	S = concor - discon
	num.perms = concor+discon
	var_s = (2*num.perms*(sqrt(8*num.perms+1)+6))/18
	
	z = qnorm(1-(1-pval)/2)
	
	n = length(slopes)
	c = z*sqrt(var_s)
	
	high = slopes[(n+c)/2]
	low  = slopes[(n-c)/2] 
	return(c(high,low))
}


sens.pval.mod = function(slopes, to.weaken=TRUE){
	
	slopes = sort(slopes)
	concor = sum(slopes > 0)
	discon = sum(slopes < 0)
	if(to.weaken){
		if(concor > discon){
			discon = discon + sum(slopes==0)
		}else{
			concor = concor + sum(slopes==0)
		}
	}
	
	S = concor - discon
	num.perms = concor+discon
	
	var_s = (2*num.perms*(sqrt(8*num.perms+1)+6))/18
	
	z = S/sqrt(var_s)
	
	pval = 1-pnorm(z)
	return(pval)
	
}


