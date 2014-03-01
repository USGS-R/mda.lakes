# find target lake in distribution

WBIC <- '1835300'

plot.fig9.GCB  <-	function(years){
  
  cex.box = 0.8
  cex.ttl = 0.8
  tck	<-	0.02
  plt.rng.x	<-	c(1978.8,2011.2)
  plt.rng.y	<-	data.frame("onset"=c(83,154),"july"=c(18.5,28.5))
  tick.x	<-	seq(1960,2020,5)
  tick.y	<-	data.frame("onset"=c(NA, 80,100,120,140,160,NA),"july"=c(18,20,22,24,26,28,30))
  plot_colors <<- c("grey40", "grey80","black","firebrick")
  lab.perc	<<-	12 # label (e.g., (a) spacing percent from corner)
  par.mgp	<<-	data.frame(x=c(1.2,.1,0),y=c(1.2,.1,0))
  plot.order	<<-	c("medium","small","large")
  line.wd	<<-	2	# plot line width
  
  fig.w	<-	3.14961
  pan.h <<- 1
  v.spc	<-	0.05 # inches of vertical space to separate panels (top only)
  h.spc	<-	0.05 # inches of horizontal space to separate panels (right only)
  l.mar	<-	0.0
  r.mar	<-	0.0
  t.mar	<-	0.00
  b.mar	<-	0.15
  left.spc	<-	0.27 #0.1
  pan.w <<- fig.w-left.spc-h.spc
  fig.h	<-	pan.h*2+v.spc*2+b.mar+t.mar
  png(filename = "../Figure_09.png",
      width = fig.w, height = fig.h, units = "in", res=300)
  
  divs  <-	5
  panels = NULL
  for (j in 1:2){	
    for (i in 1:divs){
      panels	<-	rbind(panels,c(j,j))
    }
  }
  panels	<-	rbind(panels,c(j,j))# last one!
  panels
  num.pan = 7
  

  layout(panels)
  
  par(mai=c(0,left.spc, v.spc, h.spc),mgp=par.mgp$x,omi=c(0,l.mar,t.mar,r.mar))
  plot.july(years=seq(1979,2011,1),col,plt.rng.x,plt.rng.y,cex.ttl,cex.box,tick.x,tick.y,label,tck,tick.x.lab)
  par(mai=c(fig.h/((divs*2)+1),left.spc, v.spc, h.spc))
  plot.onset(years=years,col,plt.rng.x,plt.rng.y,cex.ttl,cex.box,tick.x,tick.y,label,tck,tick.x.lab)
  #plot.onset(year,col,plt.rng.x,plt.rng.y,cex.ttl,cex.box,tick.x,tick.y,label,tck,tick.x.lab)
  #
  
  
  dev.off()
}


getStratRnk <- function(WBIC='1835300',year=1982){
  file.in <- paste('../supporting files/strat.onset',year,'.tsv',sep='')
  dat <- read.table(file.in,sep='\t',header=T)
  
  rmv.i = is.na(dat$strat.onset.DoY)
  dat <- dat[!rmv.i, ]
  srt <- sort.int(dat$strat.onset.DoY,na.last=NA,index.return = T)
  
  WBIC.rnk <- dat$WBIC[srt$ix]
  WBIC.val <- dat$strat.onset.DoY[srt$ix]
  peak.val = tail(WBIC.val,1)
  target.rnk <- which(WBIC.rnk==WBIC)
  WBIC.val <- WBIC.val[target.rnk]
  tot.strat = length(srt$ix)
  stratPerc <- target.rnk/tot.strat
  return(list(stratP=stratPerc,tot.num=WBIC.val,peak=peak.val))
}


plot.july  <-	function(years,col,plt.rng.x,plt.rng.y,cex.ttl,cex.box,tick.x,tick.y,label,tck,tick.x.lab){
  #source('GLM.functions.R')
 target = '1835300'
  #par(mgp=c(.9,.06,0))
  plot(c(0,1),c(0,1), type="l", col=NA, 
       axes=F,
       ylim=plt.rng.y$july, xlim=plt.rng.x,
       ylab="July temperature (°C)",
       xlab=NA,
       xaxs="i", yaxs="i",cex.lab=cex.ttl)
  

  sens.table	<-	read.delim("../supporting files/omg.huge.output.tsv",sep='\t',header=T)
  names(sens.table)
  
  x.vals = years
  y.vals = years*NA
  y.vals.1 = y.vals
  y.vals.2 = y.vals
  target.lake = y.vals
    quantile
  for (i in 1:length(x.vals)){
    use.i = sens.table$year==years[i]
    use.lk = sens.table$year==years[i] & sens.table$lakeid==target
    y.vals[i] <- median(sens.table$mean_surf_jul[use.i],na.rm=T)
    y.vals.1[i] <- quantile(x=sens.table$mean_surf_jul[use.i],probs=c(.25,.75),na.rm=T)[[1]]
    y.vals.2[i] <- quantile(x=sens.table$mean_surf_jul[use.i],probs=c(.25,.75),na.rm=T)[[2]]
    target.lake[i] <- sens.table$mean_surf_jul[use.lk]
  }

  polygon(x=c(x.vals,rev(x.vals)), y=c(y.vals.1,rev(y.vals.2)),
         col = plot_colors[2],border=NA)
  
 lines(x.vals,y.vals,col=plot_colors[1],type='l',lwd=1.2,lty="dotted")
  lines(x.vals,target.lake,col=plot_colors[3],type='l',lwd=line.wd)
  label.loc  <-  get.text.location(par())
  text(label.loc[1],label.loc[2],'(a)')
  par(mgp=par.mgp$y)
  axis(1,las=1, at=tick.x,cex.axis=cex.box, tck=tck,labels=NA)
  axis(3,at=tick.x,las=1, cex.axis=cex.box, tck=tck,labels=NA)
  par(mgp=par.mgp$x)
  axis(2,las=1, at=tick.y$july,cex.axis=cex.box, tck=tck)
  axis(4,at=tick.y$july,las=1, cex.axis=cex.box, tck=tck,labels=NA)  
}

plot.onset  <-  function(years=seq(1979,1988,1),col,plt.rng.x,plt.rng.y,cex.ttl,cex.box,tick.x,tick.y,label,tck,tick.x.lab){
  #source('GLM.functions.R')
  target = '1835300'
  #par(mgp=c(.9,.06,0))
  plot(c(0,1),c(0,1), type="l", col=NA, 
       axes=F,
       ylim=plt.rng.y$onset, xlim=plt.rng.x,
       ylab="Stratification onset (DoY)",
       xlab=NA,
       xaxs="i", yaxs="i",cex.lab=cex.ttl)
  
  x.vals = years
  y.vals = years*NA
  y.vals.1 = y.vals
  y.vals.2 = y.vals
  target.lake = y.vals
  
  for (i in 1:length(x.vals)){
    file.name = paste('../supporting files/strat.onset',years[i],'.tsv',sep='')
    sens.table  <-	read.delim(file.name,sep='\t',header=T)
    use.lk = sens.table$WBIC==target
    y.vals[i] <- median(sens.table$strat.onset.DoY,na.rm=T)
    y.vals.1[i] <- quantile(x=sens.table$strat.onset.DoY,probs=c(.25,.75),na.rm=T)[[1]]
    y.vals.2[i] <- quantile(x=sens.table$strat.onset.DoY,probs=c(.25,.75),na.rm=T)[[2]]
    target.lake[i] <- sens.table$strat.onset.DoY[use.lk]
  }

  polygon(x=c(x.vals,rev(x.vals)), y=c(y.vals.1,rev(y.vals.2)),
          col = plot_colors[2],border=NA)
  
  lines(x.vals,y.vals,col=plot_colors[1],type='l',lwd=1.2,lty="dotted")
  lines(x.vals,target.lake,col=plot_colors[3],type='l',lwd=line.wd)
  label.loc  <-	get.text.location(par())
  text(label.loc[1],label.loc[2],'(b)')
  
  par(mgp=par.mgp$y)
  axis(1,las=1, at=tick.x,cex.axis=cex.box, tck=tck)
  axis(3,at=tick.x,las=1, cex.axis=cex.box, tck=tck,labels=NA)
  par(mgp=par.mgp$x)
  axis(2,las=1, at=tick.y$onset,cex.axis=cex.box, tck=tck)
  axis(4,at=tick.y$onset,las=1, cex.axis=cex.box, tck=tck,labels=NA)  
  
}

get.text.location  <-	function(par,perc=10){
  x.lim	<-	par$usr[1:2] # limits
  y.lim	<-	par$usr[3:4]
  # upper right hand
  y.range	<-	y.lim[2]-y.lim[1]
  x.range <-	x.lim[2]-x.lim[1]
  
  y	<-	y.lim[2]-y.range*perc/100
  x	<-	x.lim[1]+x.range*perc/100*(pan.h/pan.w)
  return(c(x,y))
  
}

plot.fig9.GCB(years=seq(1979,2011,1))