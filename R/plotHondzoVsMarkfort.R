allUniqueVal <- function(){
  # open all the files, find the unique WBICs across all
  val.dir <- '../GLM/Run/'
  f.names <- c('Hondzo','SRTM','Simard','GLiHT','ASTER_pos','ASTER')
  
  un.wbic <- c()
  for (i in 1:length(f.names)){
    f.name = paste0(val.dir,'summary_hs.',f.names[i],'.txt')
    dat <- read.table(f.name,header=F,sep='\t')
    wbics <- dat[, 1]
    un.wbic = unique(c(un.wbic,wbics))
  }
  return(un.wbic)
}


getLC <- function(wbic){
  f.name <- '../supporting files/multi-hs_landcover.csv'
  dat <- read.table(f.name,header=T,sep=',')
  LC <- vector(length=length(wbic))
  for (i in 1:length(wbic)){
    u.i <- wbic[i]==dat$WBDY_WBIC
    if (sum(u.i) > 1){
      if (length(unique(dat$MAJORITY_landcover[u.i])) > 1){stop("WBIC duplicates with different LC vals")}
    }
    LC[i] <- dat$MAJORITY_landcover[u.i][1]
  }
  return(LC)
}

getAreas <- function(wbic){
  areas <- vector(length=length(wbic))
  source('Libraries/GLM.functions.R')
  for (i in 1:length(wbic)){
    areas[i] <- getArea(as.character(wbic[i]))
  }
  return(areas)
}


getErrors <- function(wbics){
  max.all.e <- 3.#3.5 # if all are above this RMSE, skip the lake
  # creates data.frame for box plots
  # open all the files, find the unique WBICs across all
  val.dir <- '../GLM/Run/'
  
  
  
  d.f <- data.frame(c())
  f.names <- c('Hondzo','SRTM','Simard','GLiHT','ASTER_pos','ASTER')
  
  if (length(wbics)==0){
    df = matrix(NA,nrow=1,ncol=length(f.names))
    df = data.frame(df)
    names(df) <- f.names
    return(df)
  }
  for (i in 1:length(f.names)){
    ded.vec <- vector(length=length(wbics))
    
    f.name = paste0(val.dir,'summary_hs.',f.names[i],'.txt')
    dat <- read.table(f.name,header=F,sep='\t')
    wbics.err <- dat[, 1]
    
    for (j in 1:length(wbics)){
      match.i <- wbics[j] == wbics.err
      print(sum(match.i))
      if (!is.na(sum(match.i)) & sum(match.i)> 0){
        ded.vec[j] <- mean(dat[match.i, 2])
      } else {
        ded.vec[j] <- NA
      }
    }
    cat('fail?')
    d.f.add <- data.frame(ded.vec)
    
    names(d.f.add) <- f.names[i]
    if (i == 1){
      d.f <- d.f.add
    } else {
      d.f <- cbind(d.f, d.f.add)
    }
    cat(i)
    cat('..no\n')
  }
  # will be NA padded
  
  # remove > max.all.e
  rmv.i = vector(length=length(wbics))
  for (j in 1:length(wbics)){
    bunk.i = d.f[j, ] > max.all.e | is.na(d.f[j, ])
    if (all(bunk.i)){
      rmv.i[j] = T
    }
  }
  df <- d.f[!rmv.i, ]
  return(df)
  
}

getLCkey <- function(lc.name){
  num.keys <- list('urban'=100,'agricultural'=110,'grassland'=150,'forest'=160,'water'=200,'wetland'=210)
  return(num.keys[[lc.name]])
}

plotBox <- function(land.key='agricultural',area.cuts=c(1,10)){
  #errors is a data.frame
  #areas in km2
  tot.w = 0.8

  land.type <- getLCkey(land.key)
  ar.lim <- area.cuts*1000000 # now m2
  
  errors.1 <- getErrors(un.wbic[land.cover==land.type & !is.na(land.cover) & areas<ar.lim[1]])
  errors.2 <- getErrors(un.wbic[land.cover==land.type & !is.na(land.cover) & areas>=ar.lim[1] & areas < ar.lim[2]])
  errors.3 <- getErrors(un.wbic[land.cover==land.type & !is.na(land.cover) & areas>=ar.lim[2]])

  pl <- boxplot(errors.1,ylim=c(0,6),plot=F)
  plot(c(0,NA),c(NA,NA),ylim=c(0,4.5),
       xlim=c(0.5,(length(pl$names)+.5)),
       xlab=NA,ylab="RMSE (°C)",
       axes=F,xaxs="i", yaxs="i",main=land.key)
  
  axis(side=1,at=seq(0,(length(pl$names)+1)),labels=c(NA,pl$names,NA))
  axis(side=2,at=seq(0,10))
  polyplot(pl,color='grey80',cent.off=-tot.w/3,bin=tot.w/6)
    
  pl <- boxplot(errors.2,ylim=c(0,6),plot=F)
  polyplot(pl,color='red',cent.off=0,bin=tot.w/6)
  
  pl <- boxplot(errors.3,ylim=c(0,6),plot=F)
  polyplot(pl,color='blue',cent.off=+tot.w/3,bin=tot.w/6)
}

polyplot <- function(pl,color,cent.off,bin){
  for (i in 1:length(pl$names)){
    cent <- i+cent.off
    polygon(c(cent-bin,cent-bin,cent+bin,cent+bin),c(pl$stats[2,i],pl$stats[4,i],pl$stats[4,i],pl$stats[2,i]),col=color,lwd=1.5)
    lines(c(cent-bin,cent+bin),c(pl$stats[3,i],pl$stats[3,i]),col="black",lwd=3)
    text(cent,pl$stats[2,i],pl$n[i],pos=1,cex=0.75)
  }
    
}

un.wbic <<- allUniqueVal()
land.cover <<- getLC(un.wbic)
areas <<- getAreas(un.wbic)

run.types = c('urban','agricultural','grassland','forest','water','wetland')
area.cuts=c(0.5,1)

panels = NULL
for (j in 1:3){	
  for (i in 1:3){
    panels	<-	rbind(panels,c(j,j+3))
  }
}

layout(panels)

for (i in 1:length(run.types)){
  plotBox(run.types[i],area.cuts)
}
