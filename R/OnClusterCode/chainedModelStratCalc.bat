::R CMD BATCH fixMet.R
::glm.exe

:: We need a temporary library folder
mkdir rLibs
R CMD BATCH run.calc.strat.condor.R
::R CMD BATCH condor.output.calice.R
:: Cleanup!



del *.nc
 
del.RData
del hdf5*
del nc*
del zlib*
del msvcp100.dll
del msvcr100.dll
del netcdf.dll
del szip.dll
del libcurl.dll

:: Cleanup NC library detritus

del hdf5dll.dll
del hdf5_cppdll.dll
del hdf5_f90cstubdll.dll
del hdf5_fortrandll.dll
del hdf5_hldll.dll
del hdf5_hl_cppdll.dll
del hdf5_hl_f90cstubdll.dll
del hdf5_hl_fortrandll.dll
del hdf5_toolsdll.dll
del libcurl.dll
del msvcp100.dll
del msvcr100.dll
del nc-config
del nccopy.exe
del ncdump.exe
del ncgen.exe
del ncgen3.exe
del netcdf.dll
del szip.dll
del todel.txt
del zlib.dll
del zlib1.dll

