sens.confint.mod = function(slopes, pval=0.95){
	
	slopes = sort(slopes)
	concor = sum(slopes > 0)
	discon = sum(slopes < 0)
	S = concor - discon
	num.perms = concor+discon
	var_s = (2*num.perms*(sqrt(8*num.perms+1)+6))/18
	#var_s_hirsch = sum(n.vals*(n.vals-1)*(2*n.vals + 5)/18)
	
	
	z = qnorm(1-(1-pval)/2)
	
	n = length(slopes)
	c = ceiling(z*sqrt(var_s))
	high.i = (n+c)/2
	low.i  = (n-c)/2
	
	if(high.i > n){
		high = Inf
	}else{
		high = slopes[high.i]
	}
	
	if(low.i < 1){
		low = -Inf
	}else{
		low = slopes[low.i]
	}
	
	return(c(high,low))
}

sens.confint.mod.hirsch = function(slopes, n.vals, pval=0.95){
	
	slopes = sort(slopes)
	concor = sum(slopes > 0)
	discon = sum(slopes < 0)
	S = concor - discon
	num.perms = concor+discon
	var_s = (2*num.perms*(sqrt(8*num.perms+1)+6))/18
	
	n.vals = data.table(n.vals)
	n.vals = unique(n.vals[, list(wbic,week,depth,n.obs)])
	n.vals = n.vals$n.obs
	
	var_s_hirsch = sum(n.vals*(n.vals-1)*(2*n.vals + 5)/18)
	
	z = qnorm(1-(1-pval)/2)
	
	n = length(slopes)
	c = ceiling(z*sqrt(var_s_hirsch))  #err on the high side
	high.i = (n+c)/2
	low.i  = (n-c)/2
	
	if(high.i > n){
		high = Inf
	}else{
		high = slopes[high.i]
	}
	
	if(low.i < 1){
		low = -Inf
	}else{
		low = slopes[low.i]
	}
	
	return(c(high,low))
}


sens.pval.mod.hirsch = function(slopes, n.vals, to.weaken=TRUE){
	
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
	var_s_hirsch = sum(n.vals*(n.vals-1)*(2*n.vals + 5)/18)
	
	z = S/sqrt(var_s_hirsch)
	
	pval = 1-pnorm(z)
	return(pval)
	
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

