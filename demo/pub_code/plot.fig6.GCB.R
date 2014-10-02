# get year var, get lake var

plot.fig6.GCB  <-	function(hist.all){
  lW = 3
  cex.box = 1
  cex.ttl = 1
  tck	<-	0.02
  plt.rng	<<- list(x.jul=c(0,19),x.strat=c(0,19),x.89=c(0,19),y=c(0,37))
  seq.range = seq(70,365,1)
  plot_colors <- c("black", "black", "grey80","grey40")
  
  fig.w	<-	3.14961
  pan.gap <- 0.05 # inches space between two panels
  r.mar	<-	0.05
  t.mar	<-	0.05
  b.mar <- 0.3
  left.spc	<-	0.4 # far left margin spacing
  pan.size	<-	(fig.w - left.spc - r.mar - pan.gap)/2
  divs	<-	4
  par.mgp <<- c(1.05,.14,0)
  
  bot.buffer	<-	pan.size/divs	# inches at base to allow 
  fig.h	<-	pan.size+b.mar+t.mar

  
  pdf(file = "../Figure_06.pdf",title='Read et al. figure',
      width=fig.w, height=fig.h)
  
  #hist.all <- get.hist()
  par(mai=c(b.mar,left.spc, t.mar, r.mar+pan.size+pan.gap),mgp=par.mgp,omi=c(0,0,0,0),ps = 10, cex = 1, cex.main = 1)#
  plot.jul.hist(vals=hist.all$jul.temp,plt.rng,cex.ttl,cex.box,tck,label='(a)')
  par(mai=c(b.mar,pan.gap+left.spc+pan.size, t.mar, r.mar),mgp=par.mgp,omi=c(0,0,0,0),new = TRUE)
  plot.strat.hist(vals=hist.all$strat.onset,plt.rng,cex.ttl,cex.box,tck,label='(b)')
  #plot.rank(rank.one=hist.all$rank.jul,rank.two=hist.all$rank.onset)
  
  
  dev.off()

}

plot.rank <- function(rank.one,rank.two){
  
  # may have different lengths, rank.two should be shortest
  if (length(rank.one) < length(rank.two)){
    stop("if vectors are unequal, the second must be shorter")
  }
  
  rank.two.nums = seq(1,length(rank.two),1)
  rank.one.nums = rank.two.nums*NA
  rmv.i = vector(length=length(rank.one))
  for (i in 1:length(rank.one)){
    if (rank.one[i] %in% rank.two){
      rmv.i[i] = F
    } else {
      rmv.i[i] = T
    }
  }
  rank.one = rank.one[!rmv.i]
  
  #rank.one[1:10] = rank.two[1:10]
  
  for (i in 1:length(rank.one)){
    lake.id = rank.two[i]
    rank.one.nums[i] = which(rank.one==lake.id)
  }
  plot(c(NA,1),c(0,1), type="l", col=NA, 
       axes=F,
       ylim=c(0,1000), xlim=c(0,1000),
       xlab="bla",
       xaxs="i", yaxs="i",cex.lab=1)  # faux plot axes=F,
  
  points(rank.one.nums,rank.two.nums,pch=4)
}

plot.jul.hist <- function(vals,plt.rng,cex.ttl,cex.box,tck,label){
  
  
  x.lab <- "CV July temp."
  plot(c(NA,1),c(0,1), type="l", col=NA, 
       axes=F, xlab=NA,
       ylim=plt.rng$y, xlim=plt.rng$x.jul,
       ylab="Frequency (%)",
       xaxs="i", yaxs="i",cex.lab=cex.ttl)  # faux plot axes=F,
  
  
  add.polys(vals=vals,breaks=seq(0,100,0.5),skip.zero=T)
  
  label.loc  <-  get.text.location(par())
  text(label.loc[1],label.loc[2],label,cex.lab=cex.ttl)
  
  old.par <- par()
  suppressWarnings(par(mgp = c(.55,-.15,0)))
  axis(1,at=seq(0,40,5),las=1, cex.axis=cex.box, tck=tck)
  title(xlab=x.lab,cex.lab=cex.ttl)
  suppressWarnings(par(mgp=old.par$mgp))
  axis(3,at=seq(0,40,5),las=1, cex.axis=cex.box, tck=tck,labels=NA)
  axis(2,at=seq(-100, 50, 10),las=1, cex.axis=cex.box, tck=tck)
  axis(4,at=seq(-100, 50, 10),las=1, cex.axis=cex.box, tck=tck,labels=NA)
  
}

plot.89.hist <- function(vals,plt.rng,cex.ttl,cex.box,tck,label){
  

  x.lab <- expression(paste(sigma, " of DoY > 8.9° C"))
  x.lab <- "SD of DoY > 8.9° C"
  plot(c(NA,1),c(0,1), type="l", col=NA, 
       axes=F, xlab=NA,
       ylim=plt.rng$y, xlim=plt.rng$x.89,
       ylab="Frequency (%)",
       xaxs="i", yaxs="i",cex.lab=cex.ttl)  # faux plot axes=F,
  
  
  add.polys(vals=vals,breaks=seq(0,25,1),skip.zero=T)
  
  label.loc  <-	get.text.location(par())
  text(label.loc[1],label.loc[2],label,cex.lab=cex.ttl)
  
  old.par <- par()
  suppressWarnings(par(mgp = c(.55,-.15,0)))
  axis(1,at=seq(0,40,10),las=1, cex.axis=cex.box, tck=tck)
  title(xlab=x.lab,cex.lab=cex.ttl)
  suppressWarnings(par(mgp=old.par$mgp))
  axis(3,at=seq(0,40,10),las=1, cex.axis=cex.box, tck=tck,labels=NA)
  axis(2,at=seq(-100, 50, 10),las=1, cex.axis=cex.box, tck=tck)
  axis(4,at=seq(-100, 50, 10),las=1, cex.axis=cex.box, tck=tck,labels=NA)

}

plot.strat.hist <- function(vals,plt.rng,cex.ttl,cex.box,tck,label){
  
  x.lab <- expression(paste(sigma, " of DoY stratified"))
  x.lab <- "CV of strat. onset"
  plot(c(NA,1),c(0,1), type="l", col=NA, 
       axes=F, xlab=NA,
       ylim=plt.rng$y, xlim=plt.rng$x.strat,
       ylab=NA,
       xaxs="i", yaxs="i",cex.lab=cex.ttl)  # faux plot axes=F,
  
  
  suppressWarnings(par(mgp = c(.55,-.15,0)))
  add.polys(vals=vals,breaks=seq(0,100,.5),skip.zero=T)
    
  label.loc  <-  get.text.location(par())
  text(label.loc[1],label.loc[2],label,cex.lab=cex.ttl)
  
  old.par <- par()
  suppressWarnings(par(mgp = c(.55,-.15,0)))
  axis(1,at=seq(0,40,5),las=1, cex.axis=cex.box, tck=tck)
  title(xlab=x.lab,cex.lab=cex.ttl)
  suppressWarnings(par(mgp=old.par$mgp))
  axis(3,at=seq(0,40,5),las=1, cex.axis=cex.box, tck=tck,labels=NA)
  axis(2,at=seq(-100, 50, 10),las=1, cex.axis=cex.box, tck=tck,labels=NA)
  axis(4,at=seq(-100, 50, 10),las=1, cex.axis=cex.box, tck=tck,labels=NA)
}

add.polys <- function(vals,breaks,skip.zero=T){
  if (missing(breaks)){
    breaks = seq(0,25,1)
  }
  if (skip.zero){
    vals <- vals[vals!=0]
  }
  
  hist.vals<- hist(x=vals,breaks=breaks,plot=F)
  
  print(min(vals))
  print(max(vals))
  print(median(vals))
  
  d.all <- sum(hist.vals$density)
  
  for (i in 1:length(hist.vals$mids)){
    x.poly = c(hist.vals$mids[i],hist.vals$mids[i+1],hist.vals$mids[i+1],hist.vals$mids[i])
    y.poly = c(0,0,hist.vals$density[i]*100/d.all,hist.vals$density[i]*100/d.all)

    if (any(y.poly!=0)){
      polygon(x=x.poly, y=y.poly,col="grey80",border='black',lwd=1)
    }
    
  }
}
get.text.location  <-	function(par,perc=10){
  x.lim	<-	par$usr[1:2] # limits
  y.lim	<-	par$usr[3:4]
  # upper right hand
  y.range	<-	y.lim[2]-y.lim[1]
  x.range <-	x.lim[2]-x.lim[1]
  
  y	<-	y.lim[2]-y.range*perc/100
  x	<-	x.lim[1]+x.range*perc/100
  return(c(x,y))
  
}

get.hist <- function(){
  dat = read.table('../supporting files/omg.huge.output.tsv',sep='\t',header=T)
  
  library(raster)
  names(dat)
  years <- unique(dat$year)
  lakes <- unique(dat$lakeid)
  num.lakes <- length(lakes)
  num.years <- length(years)
  
  
  year.var.89 <- vector(length=num.lakes) # for each lake, variability among years for 8.9°C exceed
  year.var.jul <- vector(length=num.lakes) # for each lake, variability among years for 8.9°C exceed
  year.var.so <- vector(length=num.lakes) # for each lake, variability among years for strat onset
  
  
  strat.mat <- matrix(nrow=num.years,ncol=num.lakes)
  
  for (j in 1:num.years){
    file.strat = paste('../supporting files/strat.onset',years[j],'.tsv',sep='')
    strat = read.table(file.strat,sep='\t',header=T)
    for (i in 1:num.lakes){
      # this is gonna be slow...
      match.i <- lakes[i] == strat$WBIC
      if (any(match.i)){
        strat.mat[j, i] <- strat$strat.onset.DoY[match.i]
      }
      
    }
    
  }
  
  for (i in 1:num.lakes){
    
    use.i <- dat$lakeid==lakes[i]
    year.var.jul[i] <- cv(dat$mean_surf_jul[use.i])#mean_surf_jul
    strats = strat.mat[, i]
    if (lakes[i]==1835300){
      
      print('big muskie strat:')
      print(cv(strat.mat[, i],na.rm=T))
      print('big muskie jul:')
      print(year.var.jul[i])
    }
      
    if (sum(is.na(strats))<10){
      year.var.so[i] <- cv(strat.mat[, i],na.rm=T)
    }
    #year.var.so[i] <- mean(dat$days_19_23[use.i])#mean_surf_jul #TEST dateOver16.7
  }
  
  # sort index
  
  srt <- sort.int(year.var.jul,index.return=T)
  sort.jul <- lakes[srt$ix]
  
  srt <- sort.int(year.var.so,index.return=T)
  sort.onset <- lakes[srt$ix]
  sort.onset = sort.onset[srt$x!=0] # remove lakes that didn't stratify
  return(list(strat.onset=year.var.so,jul.temp=year.var.jul,rank.jul=sort.jul,rank.onset=sort.onset))
}
hist.all <- get.hist()
#plot.rank(rank.one=hist.all$rank.jul,rank.two=hist.all$rank.onset)
plot.fig6.GCB(hist.all)