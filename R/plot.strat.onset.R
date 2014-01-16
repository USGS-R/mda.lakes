require(ggplot2)
year	<-	"1996"
lW = 3
cex.box = 1.8
cex.ttl = 1.8
plt.rng.x	= c(75,140)
seq.range = seq(70,365,1)

DoY.file	<-	paste("../supporting files/strat.onset",year,'.tsv',sep='')
air.file	<-	"../supporting files/obs_Tair_WI_state.csv"


# ---- for DoY over 8.9° C -----
plot_colors <- c("black", "firebrick", "grey20")


png(filename = paste("../",year,"onset",".png",sep=''),
    width = 8, height = 7.25, units = "in", res=200)

    

# Trim off excess margin space (bottom, left, top, right)
par(mai=c(1, 1, .25, 1))

# Graph autos using a y axis that uses the full range of value
# in autos_data. Label axes with smaller font and use larger 
# line widths.
plot(c(NA,1),c(0,1), type="l", col=plot_colors[1], 
	axes=F, ann=T, xlab="Day of year stratification onset",
	ylim=c(0,100), xlim=plt.rng.x,
	ylab="Percentage of lakes (%)", cex.lab=cex.ttl, lwd=lW,
	xaxs="i", yaxs="i") 

par()
# Make x axis tick marks without labels
axis(1,at=seq(10, 200, 10),las=1, cex.axis=cex.box, lwd=lW)


# Plot y axis with smaller horizontal labels 
axis(2, las=1, cex.axis=cex.box, lwd=lW)

# Create box around plot
box(lwd=lW)

# coldest to warmest ordered by july mean surface temp (Median of all)
year.order = c("2009","2002")



# --- read in air temps for the year, plot on second axis ---

air.temp	<-	read.delim(air.file,sep=',')

air.dates	<-	as.POSIXct(air.temp$TIMESTEP,origin='UTC')
air.temps	<-	air.temp$MEAN.C.
# truncate according to year...
time.1	<-	paste(year,'-1-1',sep='')
time.2	<-	paste(year,'-12-31 23:59',sep='')
use.idx	<-	which(air.dates>=as.POSIXct(time.1,origin='UTC') & air.dates < as.POSIXct(time.2,origin='UTC'))
air.dates	<-	as.numeric(strftime(air.dates[use.idx], format = "%j"))
air.temps	<-	air.temps[use.idx]



DoY	<-	read.delim(DoY.file,sep='\t')



# warm median --
dg = hist(DoY$strat.onset.DoY,seq.range,plot=FALSE)
lines(dg$breaks[-1]+diff(seq.range[1:2])*.5,cumsum(dg$counts/length(DoY$strat.onset.DoY))*100,type="l", lwd=5, #
       	col=plot_colors[1]) # COUNTS NANS!

# plot air temp
par(new=TRUE)
plot(c(NA,1),c(0,1),
	axes = FALSE, bty = "n", xlab = "", ylim=c(-11,24), 
	xlim=plt.rng.x, cex.lab=cex.ttl,
	ylab="",lwd=lW, xaxs="i", yaxs="i") 
	
lines(air.dates,air.temps,type="l", lwd=5,
		col=plot_colors[2],lty="dotted")
		
		
		
axis(side=4, at = seq(-20,40,10), cex.axis= cex.ttl, lwd=lW)
mtext("Air temperature (°C)",side=4, ,line=3, cex = cex.ttl)


# Turn off device driver (to flush output to PDF)
dev.off()

