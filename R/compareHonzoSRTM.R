source('plotHondzoVsMarkfort.R')


un.wbic <<- allUniqueVal()
land.cover <<- getLC(un.wbic)
areas <<- getAreas(un.wbic)


plot.types <- list('forest'=c('forest'),
                   'other'=c('urban','agricultural','grassland','wetland'))#'water','urban',

group.def <- data.frame('error.cut'=3,'class.min'=NA,'class.max'=NA)

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

match.lc <- getLCmatches(land.cover,plot.types$other)
match.ar <- getSizeMatches(areas,upper=group.def$class.max, lower=group.def$class.min)
sum(match.ar)
print(group.def)
pl <- getErrors(un.wbic[match.lc & match.ar],max.all.e=group.def$error.cut)
boxplot(pl,ylim=c(0,6))


print(t.test(pl$Hondzo,pl$SRTM,paired=T,alternative='greater'))

match.lc <- getLCmatches(land.cover,plot.types$forest)
match.ar <- getSizeMatches(areas,upper=group.def$class.max, lower=group.def$class.min)
pl <- getErrors(un.wbic[match.lc & match.ar],max.all.e=group.def$error.cut)
boxplot(pl,ylim=c(0,6))


print(t.test(pl$Hondzo,pl$SRTM,paired=T,alternative='two.side'))
