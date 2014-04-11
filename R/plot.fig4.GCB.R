plot.fig4.GCB <- function(){
  lW = 3
  cex.box = 1
  cex.ttl = 1
  tck  <<-	0.01
  rng.seq <<- seq(-5,40,5)
  plt.rng	<<- list(x=c(0,32),y=c(0,32))
  plot_colors <<- c(.25,.25,.25)
  alpha  <<-	list(all=0.02,epi=0.1,hypo=0.1)
  
  pch <<- 16
  pnt.cx <<- 0.4
  fig.w	<-	6.65354 # full width
  pan.gap <- 0.1 # inches space between two panels
  r.mar	<-	0.05
  t.mar	<-	0.05
  b.mar <- 0.4
  left.spc	<-	0.4 # far left margin spacing
  pan.size	<-	(fig.w - left.spc - r.mar - pan.gap*2)/3
  par.mgp <<- c(1.05,.14,0)

  fig.h	<-	pan.size+b.mar+t.mar
  print(fig.h)
  
  png(filename = "../Figure_04.png",
      width = fig.w, height = fig.h, units = "in", res=300)
  
  
  par(mai=c(b.mar,left.spc, t.mar, r.mar+(pan.size+pan.gap)*2),mgp=par.mgp,omi=c(0,0,0,0),ps = 10, cex = 1, cex.main = 1)#
  plot.all()
  
  par(mai=c(b.mar,pan.gap+left.spc+pan.size, t.mar, r.mar+pan.size+pan.gap),new = TRUE)
  plot.epi()
  
  par(mai=c(b.mar,left.spc+(pan.size+pan.gap)*2, t.mar, r.mar),new = TRUE)
  plot.hypo()
  
  
  dev.off()
  

  
}

plot.all <- function(){
  
  file.nm <- 'All_temp'
  
  dat   <-	read.delim(paste('../supporting files/',file.nm,'.tsv',sep=''))
  
  x.lab <- "Observed temperature (°C)"
  plot(c(NA,1),c(0,1), type="l", col=NA, 
       axes=F, xlab=NA,
       ylim=plt.rng$y, xlim=plt.rng$x,
       ylab="Modeled temperature (°C)",
       xaxs="i", yaxs="i",cex.lab=cex.ttl)  # faux plot axes=F,
  
  points(dat$observed,dat$modeled,pch=pch,cex=pnt.cx, col=rgb(plot_colors[1],plot_colors[2],plot_colors[3],
                                                              alpha$all,maxColorValue=1))
  abline(0,1,lwd=1,col='black')
  
  old.par <- par()
  suppressWarnings(par(mgp = c(.55,-.15,0)))
  axis(1,at=rng.seq,las=1, cex.axis=cex.box, tck=tck)
  title(xlab=x.lab,cex.lab=cex.ttl)
  suppressWarnings(par(mgp=old.par$mgp))
  axis(3,at=rng.seq,las=1, cex.axis=cex.box, tck=tck,labels=NA)
  axis(2,at=rng.seq,las=1, cex.axis=cex.box, tck=tck)
  axis(4,at=rng.seq,las=1, cex.axis=cex.box, tck=tck,labels=NA)
  
  label.loc  <-  get.text.location(par())
  text(label.loc[1],label.loc[2],"(a)",cex.lab=cex.ttl)
  
}

plot.epi <- function(){
  
  file.nm <- 'Epilimnion'
  
  dat   <-  read.delim(paste('../supporting files/',file.nm,'.tsv',sep=''))
  
  x.lab <- "Observed temperature (°C)"
  plot(c(NA,1),c(0,1), type="l", col=NA, 
       axes=F, xlab=NA,
       ylim=plt.rng$y, xlim=plt.rng$x,
       ylab=NA,
       xaxs="i", yaxs="i",cex.lab=cex.ttl)  # faux plot axes=F,
  
  points(dat$observed,dat$modeled,pch=pch,cex=pnt.cx, col=rgb(plot_colors[1],plot_colors[2],plot_colors[3],
                                                              alpha$epi,maxColorValue=1))
  abline(0,1,lwd=1,col='black')
  
  old.par <- par()
  suppressWarnings(par(mgp = c(.55,-.15,0)))
  axis(1,at=rng.seq,las=1, cex.axis=cex.box, tck=tck)
  title(xlab=x.lab,cex.lab=cex.ttl)
  suppressWarnings(par(mgp=old.par$mgp))
  axis(3,at=rng.seq,las=1, cex.axis=cex.box, tck=tck,labels=NA)
  axis(2,at=rng.seq,las=1, cex.axis=cex.box, tck=tck,labels=NA)
  axis(4,at=rng.seq,las=1, cex.axis=cex.box, tck=tck,labels=NA)
  
  label.loc  <-  get.text.location(par())
  text(label.loc[1],label.loc[2],"(b)",cex.lab=cex.ttl)
  
}
plot.hypo <- function(){
  
  file.nm <- 'Hypolimnion'
  
  dat   <-  read.delim(paste('../supporting files/',file.nm,'.tsv',sep=''))
  
  x.lab <- "Observed temperature (°C)"
  plot(c(NA,1),c(0,1), type="l", col=NA, 
       axes=F, xlab=NA,
       ylim=plt.rng$y, xlim=plt.rng$x,
       ylab=NA,
       xaxs="i", yaxs="i",cex.lab=cex.ttl)  # faux plot axes=F,
  
  points(dat$observed,dat$modeled,pch=pch,cex=pnt.cx, col=rgb(plot_colors[1],plot_colors[2],plot_colors[3],
                                                              alpha$hypo,maxColorValue=1))
  abline(0,1,lwd=1,col='black')
  
  old.par <- par()
  suppressWarnings(par(mgp = c(.55,-.15,0)))
  axis(1,at=rng.seq,las=1, cex.axis=cex.box, tck=tck)
  title(xlab=x.lab,cex.lab=cex.ttl)
  suppressWarnings(par(mgp=old.par$mgp))
  axis(3,at=rng.seq,las=1, cex.axis=cex.box, tck=tck,labels=NA)
  axis(2,at=rng.seq,las=1, cex.axis=cex.box, tck=tck,labels=NA)
  axis(4,at=rng.seq,las=1, cex.axis=cex.box, tck=tck,labels=NA)
  
  label.loc  <-  get.text.location(par())
  text(label.loc[1],label.loc[2],"(c)",cex.lab=cex.ttl)
  
}

get.text.location  <-  function(par,perc=6.5){
  x.lim	<-	par$usr[1:2] # limits
  y.lim	<-	par$usr[3:4]
  # upper right hand
  y.range	<-	y.lim[2]-y.lim[1]
  x.range <-	x.lim[2]-x.lim[1]
  
  y	<-	y.lim[2]-y.range*perc/100
  x	<-	x.lim[1]+x.range*perc/100
  return(c(x,y))
  
}

plot.fig4.GCB()