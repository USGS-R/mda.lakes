library(insol)
library(mda.lakes)
library(rLakeAnalyzer)

hypso = getBathy('1881900')

lat = 43
lon = -89.4

datetime = seq(as.POSIXct('1990-01-01'), as.POSIXct('1990-01-02'), by='hour')

metjd=JD(datetime)

sunv = sunvector(metjd, lat, lon, -6)

zenith = sunpos(sunv)[,2]

Idirdif = insolation(zenith, metjd, height=300, visibility=90, RH=70,tempK=20+273.15,O3=0.02,alphag=0.1)

cos_inc_sfc=sunv%*%as.vector(normalvector(0,0)) ## or sum(sunv*normalvector(0,0))

cos_inc_sfc[cos_inc_sfc<0]=0
Isim  = Idirdif[,1] * cos_inc_sfc

#load sparkling temps
exampleFilePath <- system.file('extdata', 'Sparkling.daily.wtr', package="rLakeAnalyzer")

#Load
sparkling.temp = load.ts(exampleFilePath)

#make up some bs light
irr = c(1:100, 100:1)

irr_thres = c(1,10)
wtr_thres = c(16,26)

area_light_threshold(0.5, irr, irr_thres, getBathy('1881900'))

area_temp_threshold(sparkling.temp, wtr_thres, getBathy('1881900'))

area_light_temp_threshold(sparkling.temp, 0.5, irr, irr_thres, wtr_thres, getBathy('1881900'))


