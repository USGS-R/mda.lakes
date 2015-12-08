# find target lake in distribution

WBIC <- '1835300'

plot.fig9.GCB  <-	function(years,corrs,plot.med=F){
  plot.med <<- plot.med
  
  fig.res = 150
  cex.box = 1.0
  cex.ttl = 1
  tck	<-	0.02
  plt.rng.x	<-	c(1978.8,2011.2)
  plt.rng.y	<-	data.frame("onset"=c(83,154),"july"=c(18.5,28.5))
  tick.x	<-	seq(1960,2020,5)
  tick.y	<-	data.frame("onset"=c(NA, 80,100,120,140,160,NA),"july"=c(18,20,22,24,26,28,30))
  plot_colors <<- c("grey30", "grey80","dodgerblue","firebrick")
  lab.perc	<<-	12 # label (e.g., (a) spacing percent from corner)
  par.mgp	<<-	data.frame(x=c(3,.2,0),y=c(3,.5,0),b=c(2.5,.8,0),co=c(.4,.15,0))
  plot.order	<<-	c("medium","small","large")
  line.wd	<<-	3	# plot line width
  font.size = 20
  
  fig.w	<-	6.75
  pan.h <<- 2.75
  v.spc	<-	0.15 # inches of vertical space to separate panels (top only)
  h.spc	<-	0.20 # inches of horizontal space to separate panels (right only)
  l.mar	<-	0.0
  r.mar	<-	0.0
  t.mar	<-	0.00
  b.mar	<-	0.1
  left.spc	<-	0.75 #0.1
  pan.w <<- (fig.w-left.spc*2-h.spc*2)/3
  pan.w <<- fig.w-left.spc-h.spc
  fig.h	<-	pan.h*2+v.spc*2+b.mar+t.mar
  
  if (plot.med){
    fig.nm = "../JAMS_Figure_07b.png"
  } else {
    fig.nm <- "../JAMS_Figure_07a.png"
  }
  png(file = fig.nm,
      width=fig.w, height=fig.h, units = "in", res=fig.res)
  
  divs  <-	9
  panels = NULL
  for (j in 1:2){	
    for (i in 1:divs){
      panels	<-	rbind(panels,c(j,j,j,j+2))
    }
  }
  panels[divs+1,4]=3
  panels	<-	rbind(panels,c(j,j,j,j+2))# last one!
  panels  <-	rbind(panels,c(j,j,j,j+2))# last one!
  panels
  

  layout(panels)
  
  par(mai=c(0,left.spc, v.spc, 0),mgp=par.mgp$x,omi=c(0,l.mar,t.mar,r.mar),ps=font.size)
  if (missing(corrs)){
    corr.july <- plot.july(years=seq(1979,2011,1),col,plt.rng.x,plt.rng.y,cex.ttl,cex.box,tick.x,tick.y,label,tck,tick.x.lab,corr=T)
  } else {
    corr.july= corrs$corr.july
    corr.onset = corrs$corr.onset
    plot.july(years=seq(1979,2011,1),col,plt.rng.x,plt.rng.y,cex.ttl,cex.box,tick.x,tick.y,label,tck,tick.x.lab,corr=F)
  }
  
  par(mai=c(fig.h/(dim(panels)[1])*2,left.spc, v.spc, 0))
  
  if (missing(corrs)){
    corr.onset <- plot.onset(years=years,col,plt.rng.x,plt.rng.y,cex.ttl,cex.box,tick.x,tick.y,label,tck,tick.x.lab,corr=T)
  } else {
    plot.onset(years=years,col,plt.rng.x,plt.rng.y,cex.ttl,cex.box,tick.x,tick.y,label,tck,tick.x.lab,corr=F)
  }
  par(mgp=par.mgp$b,mai=c(fig.h/(dim(panels)[1])*2-v.spc,left.spc*.9, v.spc, h.spc))
  plot.box(data.frame(data=corr.july),tck,cex.box,cex.ttl,xlab='July temp.',lab=' ')
  par(mgp=par.mgp$b,mai=c(fig.h/(dim(panels)[1])*2,left.spc*.9, 0, h.spc))
  plot.box(data.frame(data=corr.onset),tck,cex.box,cex.ttl,xlab='Strat. onset',lab=' ')
  
  dev.off()
  return(list(corr.july=corr.july,corr.onset=corr.onset))
}


plot.box <- function(box.data,tck,cex.box,cex.ttl,xlab,lab){
  
  t.crit <- 0.42 # ONE-tailed critical at 0.01 (testing positive correlation)
  ylabel = expression(paste("Coherence (",italic(rho),")"))
  boxplot(box.data,ylab=ylabel, axes=F,
          ylim=c(0,1),xlim=c(0,2),
          outline=F,width=.6,
          range=1,
          cex.lab=cex.ttl,lwd=1.5)#xlab=names(box.data))
  tck = tck*2
  par(mgp=par.mgp$co)
  title(xlab=xlab,cex.lab=cex.ttl)
  axis(1,las=1, at=c(-100,100),cex.axis=cex.box, tck=1e-9,labels=NA,lwd=2)
  axis(3,las=1, at=c(-100,100),cex.axis=cex.box, tck=1e-9,labels=NA,lwd=2)
  axis(2,las=1, at=seq(-1,2,.2),cex.axis=cex.box, tck=tck,lwd=2)
  axis(4,las=1, at=seq(-1,2,.2),cex.axis=cex.box, tck=tck,labels=NA,lwd=2)
  
  abline(h=t.crit,lty="1342",lwd=2)# ONE-tailed critical at 0.01 (testing positive correlation)
  
  cat(sum(box.data<0.42)); cat(' of '); cat(length(box.data[[1]])); cat(' below t.crit\n')
  
  label.loc  <-  get.text.location(par(),h=1,w=.45)
  text(label.loc[1],label.loc[2],lab)
}

plot.july  <-	function(years,col,plt.rng.x,plt.rng.y,cex.ttl,cex.box,tick.x,tick.y,label,tck,tick.x.lab,corr=F){
  #source('Libraries/GLM.functions.R')
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
  #other.lakes
  for (i in 1:length(x.vals)){
    use.i = sens.table$year==years[i]
    use.lk = sens.table$year==years[i] & sens.table$lakeid==target
    y.vals[i] <- median(sens.table$mean_surf_jul[use.i],na.rm=T)
    y.vals.1[i] <- quantile(x=sens.table$mean_surf_jul[use.i],probs=c(.25,.75),na.rm=T)[[1]]
    y.vals.2[i] <- quantile(x=sens.table$mean_surf_jul[use.i],probs=c(.25,.75),na.rm=T)[[2]]
    target.lake[i] <- sens.table$mean_surf_jul[use.lk]
  }
 if (corr){
   other.lakes = unique(sens.table$lakeid[sens.table$lakeid!=target])
   corr = vector(length=length(other.lakes))
   for (i in 1:length(other.lakes)){
     lke = other.lakes[i]
     vals.comp = vector(length=length(years))
     for (j in 1:length(x.vals)){
       use.lk = sens.table$year==years[j] & sens.table$lakeid==lke
       if (any(use.lk)){
         vals.comp[j] = sens.table$mean_surf_jul[use.lk]
       } else {
         vals.comp[j] = NA
       }
       
     }
     print(i)
     corr[i] = cor.test(y=vals.comp,x=target.lake, method = "pearson")$estimate[[1]]#summary(lm(vals.comp~target.lake))$r.squared
   }
 } else {
   corr= NA
 }
 
 if (plot.med) {
   polygon(x=c(x.vals,rev(x.vals)), y=c(y.vals.1,rev(y.vals.2)),
           col = plot_colors[2],border=NA)
   
   lines(x.vals,y.vals,col=plot_colors[1],type='l',lwd=line.wd,lty="longdash")
 }
  
  lines(x.vals,target.lake,col=plot_colors[3],type='l',lwd=line.wd)
  label.loc  <-  get.text.location(par(),h=1,w=2)
  text(label.loc[1],label.loc[2],' ')
  par(mgp=par.mgp$y)
  axis(1,las=1, at=tick.x,cex.axis=cex.box, tck=tck,labels=NA,lwd=2)
  axis(3,at=tick.x,las=1, cex.axis=cex.box, tck=tck,labels=NA,lwd=2)
  par(mgp=par.mgp$x)
  axis(2,las=1, at=tick.y$july,cex.axis=cex.box, tck=tck,lwd=2)
  axis(4,at=tick.y$july,las=1, cex.axis=cex.box, tck=tck,labels=NA,lwd=2)
 return(corr)
}



plot.onset  <-  function(years=seq(1979,1988,1),col,plt.rng.x,plt.rng.y,cex.ttl,cex.box,tick.x,tick.y,label,tck,tick.x.lab,corr=F){
  #source('Libraries/GLM.functions.R')
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
  
  # build first year
  file.name = '../supporting files/strat.onset1979.tsv'
  sens.table  <-  read.delim(file.name,sep='\t',header=T)
  other.lakes <- sens.table$WBIC[sens.table$WBIC!=target & !is.na(sens.table$strat.onset.DoY)]
  other.vals <- matrix(nrow=length(x.vals),ncol=length(other.lakes))
  
  for (i in 1:length(x.vals)){
    file.name = paste('../supporting files/strat.onset',years[i],'.tsv',sep='')
    sens.table  <-	read.delim(file.name,sep='\t',header=T)
    use.lk = sens.table$WBIC==target
    y.vals[i] <- median(sens.table$strat.onset.DoY,na.rm=T)
    y.vals.1[i] <- quantile(x=sens.table$strat.onset.DoY,probs=c(.25,.75),na.rm=T)[[1]]
    y.vals.2[i] <- quantile(x=sens.table$strat.onset.DoY,probs=c(.25,.75),na.rm=T)[[2]]
    target.lake[i] <- sens.table$strat.onset.DoY[use.lk]
    if (corr){
      for (j in 1:length(other.lakes)){
        use.i = sens.table$WBIC==other.lakes[j]
        if (any(use.i)){
          other.vals[i,j] <- sens.table$strat.onset.DoY[use.i]
        } else {
          other.vals[i,j] = NA
        }
        
      }
    }
    
  }
  if (corr){
    corr = vector(length=length(other.lakes))
    for (i in 1:length(other.lakes)){
      print(i)
      corr[i] = cor.test(y=other.vals[,i],x=target.lake, method = "pearson")$estimate[[1]]#summary(lm(other.vals[,i]~target.lake))$r.squared
    }
  } else {
    corr= NA
  }
  

  if (plot.med){
    polygon(x=c(x.vals,rev(x.vals)), y=c(y.vals.1,rev(y.vals.2)),
            col = plot_colors[2],border=NA)
    
    lines(x.vals,y.vals,col=plot_colors[1],type='l',lwd=line.wd,lty="longdash")
    
  }
  lines(x.vals,target.lake,col=plot_colors[3],type='l',lwd=line.wd)
  label.loc  <-	get.text.location(par(),h=1,w=2)
  text(label.loc[1],label.loc[2],' ')
  
  par(mgp=par.mgp$y)
  axis(1,las=1, at=tick.x,cex.axis=cex.box, tck=tck,lwd=2)
  axis(3,at=tick.x,las=1, cex.axis=cex.box, tck=tck,labels=NA,lwd=2)
  par(mgp=par.mgp$x)
  axis(2,las=1, at=tick.y$onset,cex.axis=cex.box, tck=tck,lwd=2)
  axis(4,at=tick.y$onset,las=1, cex.axis=cex.box, tck=tck,labels=NA,lwd=2)
  return(corr)
}

get.text.location  <-	function(par,perc=9,h=1,w=1){
  x.lim	<-	par$usr[1:2] # limits
  y.lim	<-	par$usr[3:4]
  # upper right hand
  y.range	<-	y.lim[2]-y.lim[1]
  x.range <-	x.lim[2]-x.lim[1]
  
  y	<-	y.lim[2]-y.range*perc/100
  x	<-	x.lim[1]+x.range*perc/100*(h/w)
  return(c(x,y))
  
}


corrs <- plot.fig9.GCB(years=seq(1979,2011,1),corrs=corrs)
corrs <- plot.fig9.GCB(years=seq(1979,2011,1),corrs=corrs,plot.med=T)