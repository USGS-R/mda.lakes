source('plotHondzoVsMarkfort.R')


un.wbic <<- allUniqueVal()
land.cover <<- getLC(un.wbic)
areas <<- getAreas(un.wbic)


plot.types <- list('Forest'=c('forest'),
                   'Other'=c('urban','agricultural','grassland','wetland'))#'water','urban',

group.def <- data.frame('error.cut'=3,'class.min'=0.1,'class.max'=10)

getLCmatches <- function(landcover,match.class){
  
  match.i = vector(length=length(landcover))
  for (i in 1:length(match.class)){
    lc = getLCkey(match.class[i])
    matches = match.i | landcover==lc & !is.na(land.cover)
    match.i = matches
  }
  
  return(match.i)
}

getSizeMatches <- function(areas,upper=NA,lower=NA){
  upperKM2 <- upper*1000000
  lowerKM2 <- lower*1000000
  
  if (!is.na(upper)){
    up.match <- areas <= upperKM2
  } else {
    up.match <- rep(T,length(areas))
  }
  
  if (!is.na(lower)){
    lw.match <- areas >= lowerKM2
  } else {
    lw.match <- rep(T,length(areas))
  }
  
  match.i <- lw.match & up.match
  return(match.i)
}

polyplot2 <- function(pl,colors,cent.off,bin,top=4.5){
  bin.c = c(cent.off+bin+.05,cent.off-bin-.05)
  bin.h = c(top-0.15,top+0.15)
  lwd = 2
  for (i in 1:length(pl$names)){
    cent <- bin.c[i]
    lines(c(cent,cent),c(pl$stats[1,i],pl$stats[5,i]),col='black',lwd=lwd)
    lines(c(cent-.05,cent+.05),c(pl$stats[1,i],pl$stats[1,i]),col='black',lwd=lwd)
    lines(c(cent-.05,cent+.05),c(pl$stats[5,i],pl$stats[5,i]),col='black',lwd=lwd)
    polygon(c(cent-bin,cent-bin,cent+bin,cent+bin),c(pl$stats[2,i],pl$stats[4,i],pl$stats[4,i],pl$stats[2,i]),col=colors[i],lwd=lwd)
    lines(c(cent-bin,cent+bin),c(pl$stats[3,i],pl$stats[3,i]),col="black",lwd=lwd)
    #text(cent,pl$stats[2,i],pl$n[i],pos=1,cex=0.75)
    polygon(c(.55,.7,.7,.55),c(bin.h[i]-0.1,bin.h[i]-0.1,bin.h[i]+0.1,bin.h[i]+0.1),
            col=colors[i],lwd=lwd-.5)
    text(.65,bin.h[i]-.03,pl$names[i],pos=4,cex=01)
  }
  
}

match.lc <- getLCmatches(land.cover,plot.types[[1]])
match.ar <- getSizeMatches(areas,upper=group.def$class.max, lower=group.def$class.min)

cex.box = 1
cex.ttl = 1
tck  <-	0.02
#plt.rng	<<- list(x.jul=c(0,19),x.strat=c(0,19),x.89=c(0,19),y=c(0,37))
#seq.range = seq(70,365,1)
#plot_colors <- c("black", "black", "grey80","grey40")

fig.w	<-	3.14961
r.mar	<-	0.1
t.mar	<-	0.1
b.mar <- 0.3
l.mar	<-	0.5 # far left margin spacing
pan.size	<-	fig.w - l.mar - r.mar
#par.mgp <<- c(1.05,.14,0)
tck  <<-  0.01
fig.h	<-	pan.size+b.mar+t.mar


png(file = "../Figure_Hondzo_SRTM.png",res=300,
    width=fig.w, height=fig.h,units = "in")
par(mai=c(b.mar,l.mar, t.mar, r.mar),mgp=par.mgp,omi=c(0,0,0,0),ps = 10, cex = 1, cex.main = 1)#

tot.w = 0.8
plot(c(0,NA),c(NA,NA),ylim=c(0,4.9),
     xlim=c(0.5,(length(plot.types)+.5)),
     xlab=NA,ylab="RMSE (°C)",
     axes=F,xaxs="i", yaxs="i")

pl <- getErrors(un.wbic[match.lc & match.ar],max.all.e=group.def$error.cut)
pl <- pl[, 1:2]
print(t.test(pl$Hondzo,pl$SRTM,paired=T,alternative='greater'))
pl <- boxplot(pl,ylim=c(0,6),plot=F)
par(mgp = c(.5,.04,0))
axis(side=1,at=seq(0,(length(plot.types)+1)),labels=c(NA,names(plot.types),NA),lwd=2,tck=tck)
par(mgp = c(0.9,.04,0))
axis(side=2,at=seq(0,10),lwd=2,tck=tck)
axis(side=3,at=c(-100,100),lwd=2,tck=tck)
axis(side=4,at=c(-100,100),lwd=2,tck=tck)

polyplot2(pl,colors=c('grey80','white'),cent.off=1,bin=tot.w/6)
match.lc <- getLCmatches(land.cover,plot.types[[2]])
match.ar <- getSizeMatches(areas,upper=group.def$class.max, lower=group.def$class.min)
pl <- getErrors(un.wbic[match.lc & match.ar],max.all.e=group.def$error.cut)
pl <- pl[, 1:2]
print(t.test(pl$Hondzo,pl$SRTM,paired=T,alternative='two.side'))
pl <- boxplot(pl,ylim=c(0,6),plot=F)
polyplot2(pl,colors=c('grey80','white'),cent.off=2,bin=tot.w/6)

dev.off()

