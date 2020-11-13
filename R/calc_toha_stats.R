

#'@title Calc standard stats on TOHA for a given lake
#'
#'
#'@inheritParams calc_toha
#'@param width_thresh The fraction of benthic area threshold across which to find the width of the peak (1 is 100 percent)
#'
#'
#'
#'
#'@export
calc_toha_stats = function(lakeid, width_thresh=0.02, secchi=c(seq(0.1,2.9, by=0.1),3:10), wtr_thresh=c(11,25), irr_thresh=c(0.0762, 0.6476)){
	
	if(! width_thresh > 0){
		stop('width_thresh must be greater than zero') 
	}
	
	toha = calc_toha(lakeid, secchi, wtr_thresh, irr_thresh)
	
	out = data.frame(lakeid=lakeid)
	
	out$pct_max = max(tmp$opti_therm_hab/tmp$lake_benthic_area)
	out$max = max(tmp$opti_therm_hab)
	
	#lets set toha to zero at secchi of 0 and a secchi of 1000
	pct_toha    = c(0, toha$opti_therm_hab/toha$lake_benthic_area, 0)
	secchi_zero = c(0, toha$secchi, 1000)
	
	sign_toha      = sign(pct_toha-width_thresh)
	
	widths  = c()
	last_abv = NA
	
	for(i in 1:length(sign_toha)){
		if(is.na(last_abv) && sign_toha[i] == 1){
			last_abv = i
		}else if(!is.na(last_abv) && sign_toha[i] == -1){
			#just went below
			#calculate width add it
			secchi_up = approx(pct_toha[(last_abv-1):last_abv], secchi_zero[(last_abv-1):last_abv], xout = width_thresh)$y
			secchi_dn = approx(pct_toha[(i-1):i], secchi_zero[(i-1):i], xout = width_thresh)$y
			width  = secchi_dn - secchi_up
			widths = c(widths, width)
			
			last_abv = NA
		}
	}
	
	out$npeaks = length(widths)
	out$peak_width = sum(widths)
	
	return(out)
}