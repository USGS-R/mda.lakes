require(ggplot2)
fig.dir	<-	'/Users/jread/Desktop/Science Projects/WiLMA/Figures/'

files	<-	c('Epilimnion','Hypolimnion','All_temp')
alpha	<-	c(0.05,0.05,0.01)

for (i in 1:length(files)){
	dat 	<-	read.delim(paste('../supporting files/',files[i],'.tsv',sep=''))


	p <- ggplot() +
	geom_abline(intercept = 0, slope = 1) +
	geom_point(data = dat, aes(x=dat[,2],dat[,1],size=3.5),colour='darkgreen',alpha=alpha[i]) + #dat[,2], dat[,1],	
	  	xlab("Observed temperature (°C)") + ylab("Modeled temperature (°C)") +
	  	theme_bw() +
	  	theme(text=element_text(family="Times", size=22),
			axis.line = element_line(size = 0.0),axis.ticks = element_line(size = 0.5), 
			panel.border = element_rect(size = 1, colour='black'),
			legend.position = "none") +
		scale_x_continuous(limits = c(0, 30),breaks = seq(0,30,5)) +
		scale_y_continuous(limits = c(0, 30),breaks = seq(0,30,5))

	ggsave(paste(fig.dir,files[i],'.png',sep=''), p, width=6, height=6)
}
