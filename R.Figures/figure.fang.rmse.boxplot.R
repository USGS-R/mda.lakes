## Fang stefan boxplot

errors = c(1.65, 1.7, 1.72, 1.7, 1.34, 1.47, 2.06, 1.76, 1.67, 1.37, 
           1.76, 1.46, 1.13, 1.66, 1.72, 0.92, 1.42, 1.68, 1.24, 0.8)

tiff('fang.rmse.boxplot.tiff', width=900, height=1500, res=300, compression='lzw')
boxplot(errors, main = "Fang et al. 2012", ylab='RMSE', ylim=c(0,2.1))
dev.off()
