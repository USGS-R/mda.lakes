#This file runs accompanies each chained condor run and is run
# at each node.


install.packages("ncdf4_1.4.zip", lib='./rLibs', repos=NULL)
install.packages("rGLM_0.1.2.tar.gz", lib='./rLibs', repos=NULL, type='source')
source('chained.GLM.lib.R')
library(rGLM)

run.chained.GLM('.', glm.path = 'glm.exe', NULL, verbose=TRUE)
