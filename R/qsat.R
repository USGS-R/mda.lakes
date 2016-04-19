#    INPUT:   Ta - air temperature  [C]
#             Pa - (optional) pressure [mb]
#
#    OUTPUT:  q  - saturation specific humidity  [kg/kg]
qsat = function(Ta, Pa){
	ew = 6.1121*(1.0007+3.46e-6*Pa)*exp((17.502*Ta)/(240.97+Ta)) # in mb
	q  = 0.62197*(ew/(Pa-0.378*ew))                              # mb -> kg/kg
	return(q)
}
