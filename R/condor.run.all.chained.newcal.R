


source('Libraries/htcondor-R.R')

origin = getwd()

wilma = 'D:/WILMA/Wilma-m/R'
files = file.path(wilma, c('Libraries/chained.GLM.lib.R', 'Libraries/GLM.functions.R', 
													 'OnClusterCode/.Renviron', 'condor.run.all.chained.newcal.oncondor.R'))

files = c(files, file.path('D:/WILMA/GLM/1.2',
												c('glm.exe', 'hdf5_hldll.dll', 'hdf5dll.dll',
													'libmmd.dll', 'netcdf.dll', 'svml_dispmd.dll', 'szip.dll',
													'zlib1.dll')))


data = file.path(wilma, c('../supporting files/empirical.ice.tsv', '../supporting files/wtemp.obs.tsv', 
				 '../supporting files/nldas.airt.coef.tsv', '../supporting files/annual_mean_secchi.txt'))

data = c(data, 'd:/WILMA/drivers.zip', 'd:/WILMA/subdaily_fixed_nmls.zip')



# sw = args[1:2] #intercept slope
# lw = args[3:4]
# rh = args[5:6]
for(i in seq(-6, -10, by=-1)){
		
	
	
	multipliers = c(i, 1,  #sw
									0, 0.97,  #lw
									0, 1)  #rh
	
	args = paste('condor.run.all.chained.newcal.oncondor.R', paste(multipliers, collapse=" "))
	
	this.run = paste('D:/WilmaRuns/2014-05-14multipliers/lw0.97swB', i, sep='')
	dir.create(this.run)
	
	if(file.exists(file.path(this.run,'all.cal.tsv'))){
		next
	}
	
	condor.write.submit(file.path(this.run,'submit.cmd'), 
											executable=file.path(wilma,'OnClusterCode/rscript_wrap.bat'), 
											input.files=c(files, data),
											arguments=args)
	
	condor.write.dag.wrap(file.path(this.run, 'simple.dag'), condor.submit = 'submit.cmd')
	
	#file.copy(c(files, data), this.run)
	
	setwd(this.run)
	
	system('condor_submit_dag simple.dag')
	
	setwd(origin)
}







