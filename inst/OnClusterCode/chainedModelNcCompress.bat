::R CMD BATCH fixMet.R
::glm.exe

:: We need a temporary library folder
mkdir rLibs
R CMD BATCH run.single.condor.R
::R CMD BATCH condor.output.calice.R
:: Cleanup!

unzip -o netcdfBins.zip
nccopy -k 4 -d 8 output1979.nc output1979.nc4
 nccopy -k 4 -d 8 output1980.nc output1980.nc4
 nccopy -k 4 -d 8 output1981.nc output1981.nc4
 nccopy -k 4 -d 8 output1982.nc output1982.nc4
 nccopy -k 4 -d 8 output1983.nc output1983.nc4
 nccopy -k 4 -d 8 output1984.nc output1984.nc4
 nccopy -k 4 -d 8 output1985.nc output1985.nc4
 nccopy -k 4 -d 8 output1986.nc output1986.nc4
 nccopy -k 4 -d 8 output1987.nc output1987.nc4
 nccopy -k 4 -d 8 output1988.nc output1988.nc4
 nccopy -k 4 -d 8 output1989.nc output1989.nc4
 nccopy -k 4 -d 8 output1990.nc output1990.nc4
 nccopy -k 4 -d 8 output1991.nc output1991.nc4
 nccopy -k 4 -d 8 output1992.nc output1992.nc4
 nccopy -k 4 -d 8 output1993.nc output1993.nc4
 nccopy -k 4 -d 8 output1994.nc output1994.nc4
 nccopy -k 4 -d 8 output1995.nc output1995.nc4
 nccopy -k 4 -d 8 output1996.nc output1996.nc4
 nccopy -k 4 -d 8 output1997.nc output1997.nc4
 nccopy -k 4 -d 8 output1998.nc output1998.nc4
 nccopy -k 4 -d 8 output1999.nc output1999.nc4
 nccopy -k 4 -d 8 output2000.nc output2000.nc4
 nccopy -k 4 -d 8 output2001.nc output2001.nc4
 nccopy -k 4 -d 8 output2002.nc output2002.nc4
 nccopy -k 4 -d 8 output2003.nc output2003.nc4
 nccopy -k 4 -d 8 output2004.nc output2004.nc4
 nccopy -k 4 -d 8 output2005.nc output2005.nc4
 nccopy -k 4 -d 8 output2006.nc output2006.nc4
 nccopy -k 4 -d 8 output2007.nc output2007.nc4
 nccopy -k 4 -d 8 output2008.nc output2008.nc4
 nccopy -k 4 -d 8 output2009.nc output2009.nc4
 nccopy -k 4 -d 8 output2010.nc output2010.nc4
 nccopy -k 4 -d 8 output2011.nc output2011.nc4
 nccopy -k 4 -d 8 output2012.nc output2012.nc4
 nccopy -k 4 -d 8 output2013.nc output2013.nc4

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

