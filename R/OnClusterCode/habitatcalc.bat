::R CMD BATCH fixMet.R
::glm.exe
::unzip -o netcdfBins.zip
::nccopy -k 4 -d 8 output.nc output.nc4

:: We need a temporary library folder
mkdir rLibs
R CMD BATCH habitat.calc.condor.R
::R CMD BATCH condor.output.calice.R
:: Cleanup!
::del output.nc

