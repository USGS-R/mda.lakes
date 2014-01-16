## This runs on the condor node to do all habitat calcs

install.packages("ncdf4_1.4.zip", lib='./rLibs', repos=NULL)
install.packages("rGLM_0.1.2.tar.gz", lib='./rLibs', repos=NULL, type='source')
install.packages('rLakeAnalyzer_1.0.zip', lib='./rLibs', repos=NULL)
install.packages('stringr_0.6.2.zip', lib='./rLibs', repos=NULL)

source('chained.habitat.out.R')
source('GLM.physics.R')

library('ncdf4')
library('rGLM')
library('rLakeAnalyzer')
library('stringr')

info = read.table('run.info.tsv', header=TRUE, sep='\t')

chained.habitat.calc('.', 'habitat.out.tsv', info$WBIC)