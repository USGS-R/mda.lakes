## This runs on the condor node to do all habitat calcs

install.packages("ncdf4_1.4.zip", lib='./rLibs', repos=NULL)
install.packages("rGLM_0.1.5.tar.gz", lib='./rLibs', repos=NULL, type='source')
install.packages('rLakeAnalyzer_1.0.zip', lib='./rLibs', repos=NULL)
install.packages('stringr_0.6.2.zip', lib='./rLibs', repos=NULL)


source('chained.GLM.lib.R')

library('ncdf4')
library('rGLM')
library('rLakeAnalyzer')
library('stringr')

info = read.table('run.info.tsv', header=TRUE, sep='\t')

wtr = get.wtr.chained.prefix('.')

write.table(wtr, 'output.wtr', sep='\t', col.names=TRUE, row.names=FALSE)


plotGLM  <- function(GLM,figName="glmPlot",folder="./",cLim=c(0,30)){
  # saves GLM plot to directory
  
  elevs <-  getElevGLM(GLM)
  lvls  <-  seq(cLim[1],cLim[2])
  figW  <-  8
  figH  <-  3.5
  lM    <-  .95
  bM    <-  .55
  rM    <-  .15
  tM    <-  0.25
  fRes  <-  300
  fontN <-  11
  xL    <-  c(as.numeric(min(GLM$DateTime)),as.numeric(max(GLM$DateTime)))
  yL    <-  c(min(elevs,na.rm=TRUE),max(elevs,na.rm=TRUE))
  cMap  <-  rev(rainbow(length(lvls),s=1,v=1,start=0,end=4/6))
  
  vals <- data.matrix(GLM)
  output = paste(folder,figName,".png", sep = "")
  png(output, width=figW, height=figH, units="in",res=fRes)
  par(mai=c(bM,lM,rM,tM),usr=c(xL[1],xL[2],yL[1],yL[2]))
  wtr <- vals[,2:(length(elevs)+1)]
  filled.contour(x=GLM$DateTime,y=elevs,z=wtr,col = cMap,
                 levels=lvls,xaxs = "i",plot.title = title(ylab = "Elevation from bottom (m)"),
                 xlim=xL, ylim=yL, xaxp = c(xL[1],xL[2],50))
  dev.off()
}

plotGLM(wtr)
