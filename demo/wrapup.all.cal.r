
################################################################################

runs.dir = 'D:/WilmaRuns/JamoCanopy'

runs = Sys.glob(file.path(runs.dir, 'WBIC_*'))

cal.dir = file.path(runs.dir, 'Cal Out')

dir.create(cal.dir)

all.cal = data.frame()

failed = 0

for(i in 1:length(runs)){
  
  cal.path = file.path(runs[i], 'cal.out.tsv')
  if(!file.exists(cal.path)){
    
    if(file.exists(file.path(runs[i], 'cal.in.tsv'))){
      failed = failed + 1
      cat('failed', runs[i],'\n')
    }
    
    next
  }
  
  tmp = read.table(file.path(runs[i], 'cal.out.tsv', sep='\t', header=TRUE))
  all.cal = rbind(all.cal, tmp)
}

cat(failed, 'runs failed')

f.out = file.path(cal.dir, paste(basename(runs.dir), '.tsv', sep=''))

write.table(all.cal, f.out, sep='\t', row.names=FALSE)
