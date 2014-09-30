## Figure Empirical Data Density
library(plyr)

wtr = read.table('../supporting files/wtemp.obs.tsv', sep='\t', header=TRUE)
wtr$DATETIME = as.POSIXct(wtr$DATETIME)
wtr$year = as.POSIXlt(wtr$DATETIME)$year+1900

data.by.year = ddply(wtr,'year',function(df) length(unique(df$WBIC)))


tiff('data.by.year.1979.tiff', width=1800, height=1200, res=300, compression='lzw')
plot(data.by.year$year, data.by.year$V1, xlim=c(1979, 2011), xlab="Year", 
     ylab="Unique Lakes", type='b', lwd=2)
dev.off()


tiff('data.by.year.1950.tiff', width=1800, height=1200, res=300, compression='lzw')
plot(data.by.year$year, data.by.year$V1, xlim=c(1950, 2011), xlab="Year", 
     ylab="Unique Lakes", type='b', lwd=2)
dev.off()


better.JAS = function(df){
  if(nrow(df) > 4){
    return(mean(df$V1))
  }else{
    return(NA)
  }
}


wtr.near.surf = ddply(wtr, c("WBIC", "DATETIME"), function(df) mean(df$WTEMP[df$DEPTH<=2], na.rm=TRUE))

mons = as.POSIXlt(wtr.near.surf$DATETIME)$mon+1

wtr.near.surf = wtr.near.surf[mons >=7 & mons <= 9,]
wtr.near.surf$year = as.POSIXlt(wtr.near.surf$DATETIME)$year+1900

JAS.mean = ddply(wtr.near.surf, c("WBIC", "year"), better.JAS)
JAS.mean = JAS.mean[!is.na(JAS.mean$V1),]

data.by.year = ddply(JAS.mean,'year',function(df) nrow(df))

tiff('data.JAS.yearly.1979.tiff', width=1800, height=1200, res=300, compression='lzw')
plot(data.by.year$year, data.by.year$V1, xlim=c(1979, 2011), xlab="Year", 
     ylab="Lakes with JAS", type='b', lwd=2)
dev.off()


