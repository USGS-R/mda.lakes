require(ggplot2)
dat<-read.table('../supporting files/omg.huge.output.tsv',header = TRUE, sep = "\t")
names(dat)


all.box = boxplot(dat$mean_surf_JAS~dat$year,plot=FALSE) # was jul

dat.2009<- dat$mean_surf_JAS
dat.2009[dat$year!=2009] = NA

dat.2002<- dat$mean_surf_JAS
dat.2002[dat$year!=2002] = NA# was 2002

dat.1996<- dat$mean_surf_JAS
dat.1996[dat$year!=1996] = NA

dat.1998<- dat$mean_surf_JAS
dat.1998[dat$year!=1998] = NA

box.dat<- data.frame("All"=dat$mean_surf_JAS,'2009'=dat.2009,'1996'=dat.1996,'2002'=dat.2002,'1998'=dat.1998)

names(box.dat)= c('All','2009','1996','2002','1998')
# ---- for july temp histograms -----
lW = 1.25
cex.box = 1.4
cex.ttl = 1.4
png(filename = "../JulyHist.png",
    width = 7.5, height = 4.5, units = "in", pointsize = 12, res=200)
box.loc <- c(1.25,2.3,2.9,3.5,4.1)

par(mar=c(2.5, 5, 0.75, 0.75))
boxplot(box.dat, range=1.5, ylim = c(17.7, 31.7),outline=FALSE, 
        lwd=lW+2, xlim = c(.75, 4.25), width=c(1.0,.5,.5,.5,.5), at=box.loc,
        ylab="Mean July surface temperature (°C)", cex.lab=cex.ttl,cex.axis=cex.box)
axis(1,lwd=lW,lab=F, at=box.loc)
axis(2,lwd=lW,lab=F)

box(lwd=lW)
# now freq hist
dev.off()


# ---- for DoY over 8.9° C -----
# Define colors to be used for cars, trucks, suvs
plot_colors <- c(rgb(r=0.0,g=0.0,b=0.9), "red", "forestgreen")

# Start PDF device driver to save output to figure.pdf
png(filename = "../DoY8.9.png",
    width = 12, height = 6, units = "in", res=200)

    

# Trim off excess margin space (bottom, left, top, right)
par(mar=c(7, 7, 0.5, 0.5))

# Graph autos using a y axis that uses the full range of value
# in autos_data. Label axes with smaller font and use larger 
# line widths.
plot(c(NA,1),c(0,1), type="l", col=plot_colors[1], 
     ylim=c(0,10), xlim=c(75,140),axes=F, ann=T, xlab="Day of year temperature exceeded 8.9° C",
     ylab="Frequency of observations (%)", cex.lab=cex.ttl, lwd=lW)

# Make x axis tick marks without labels
axis(1,las=1, cex.axis=cex.box, lwd=lW)


# Plot y axis with smaller horizontal labels 
axis(2, las=1, cex.axis=cex.box, lwd=lW)

# Create box around plot
box(lwd=lW)




seq.range = seq(50,150,1)

# coldest to warmest ordered by july mean surface temp (Median of all)
year.order = c("2009","1992","1996","1993","2004","2000","1994","1985","1984","2008","1995","1990","2003","1997","2007","1979","1982",
               "1991","2001","1986","1999","1981","2010","2005","1980","1988","1998","1989","1987","2006","1983","2011","2002")
num.years = 5

dg = hist(dat$dateOver8.9[dat$year %in% head(year.order,num.years)],seq.range,plot=FALSE)
lines(dg$breaks[-1]+diff(seq.range[1:2])*.5,dg$counts/sum(dg$counts)*100,type="l", lwd=5, 
       col=plot_colors[1])

count.cold = sum(dg$counts)

dg = hist(dat$dateOver8.9[dat$year %in% tail(year.order,num.years)],seq.range,plot=FALSE)
lines(dg$breaks[-1]+diff(seq.range[1:2])*.5,dg$counts/sum(dg$counts)*100,type="l", lwd=5, 
       col=plot_colors[2])

count.warm = sum(dg$counts)
#dg = hist(dat$dateOver8.9,seq.range,plot=FALSE)
#lines(dg$breaks[-1]+diff(seq.range[1:2])*.5,dg$counts/sum(dg$counts)*100,type="l",  lwd=3, 
#      col='black')

# Create a legend in the top-left corner that is slightly  
# smaller and has no border

title.cold = paste('Coldest ',as.character(num.years),' years (n=',as.character(count.cold),')',sep='')
title.warm = paste('Warmest ',as.character(num.years),' years (n=',as.character(count.warm),')',sep='')
legend("topleft", c(title.cold,title.warm), cex=cex.box, col=plot_colors[1:2], 
       lwd=c(5,5), bty="n");

# Turn off device driver (to flush output to PDF)
dev.off()


# ---- for temp range histograms -----

get.clear.dark	<-	function(years='All'){
	
	dat.clear <- read.table('../supporting files/clear.range.tsv',header = TRUE, sep = "\t")
	dat.dark  <- read.table('../supporting files/dark.range.tsv',header = TRUE, sep = "\t")
	num.clear	<-	dim(dat.clear)[1]
	num.dark	<-	dim(dat.dark)[1]
	num.range	<-	max(c(num.clear,num.dark))
	clear.range	<-	vector(length=num.range)	# will include padding if lengths are different
	dark.range	<-	vector(length=num.range)
	if (years=="All"){
		for (j in 1:num.clear){
			clear.range[j]	<-	max(dat.clear[j,-1],na.rm=TRUE)-min(dat.clear[j,-1],na.rm=TRUE)
		}
		for (j in 1:num.dark){
			dark.range[j]	<-	max(dat.dark[j,-1],na.rm=TRUE)-min(dat.dark[j,-1],na.rm=TRUE)
		}
	} else {
		headers = paste("X",as.character(years),sep='')
		for (j in 1:num.clear){
			clear.range[j]	<-	max(dat.clear[j,headers],na.rm=TRUE)-min(dat.clear[j,headers],na.rm=TRUE)
		}
		for (j in 1:num.dark){
			dark.range[j]	<-	max(dat.dark[j,headers],na.rm=TRUE)-min(dat.dark[j,headers],na.rm=TRUE)
		}
	}
	
	plot.frame<- data.frame('Clear'=clear.range,
	                        'Dark'=dark.range) # pad w/ NAs
	
}

lW = 3
cex.box = 1.8
cex.ttl = 1.8
png(filename = "../RangePlot.png",
    width = 6, height = 5.75, units = "in", pointsize = 12, res=200)

plot.frame	<-	get.clear.dark(c(1996,1998))
par(mar=c(4, 7, 0.5, 0.5))
boxplot(plot.frame, range=1.5, ylim = c(-1.2, 1.2),outline=FALSE, 
        lwd=lW+2, width=c(3,3),
        ylab="Mean July surface temperature (°C)", 
        cex.lab=cex.ttl,cex.axis=cex.box,yaxp=c(-1,2,3))
axis(1,lwd=lW,lab=F,at=c(1,2))
axis(2,lwd=lW,lab=F,at=seq(-1,2,1))

box(lwd=lW)
# now freq hist
dev.off()