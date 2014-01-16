::R CMD BATCH fixMet.R
::glm.exe
::unzip -o netcdfBins.zip
::nccopy -k 4 -d 8 output.nc output.nc4

:: We need a temporary library folder
mkdir rLibs
R CMD BATCH run.single.condor.R
R CMD BATCH habitat.calc.condor.R

:: Cleanup!
::del output.nc
del.RData
del hdf5*
del nc*
del zlib*
del msvcp100.dll
del msvcr100.dll
del netcdf.dll
del szip.dll
del libcurl.dll
del *.nc
del lake.csv