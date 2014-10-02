## Animated Theil-Sen slope estimator
library(animation)

to.plot = function(){
	
	par(mfrow=c(1,2))

	x = seq(1, 5, length.out=10)
	y = x*1 + rnorm(length(x))
	
	perm.i = combn(1:length(x), 2)
	slopes = rep(NA, ncol(perm.i))
	
	for(i in 1:ncol(perm.i)){
		
		slopes[i] = (y[perm.i[1,i]]-y[perm.i[2,i]])/(x[perm.i[1,i]] - x[perm.i[2,i]])
		
		plot(x, y)
		lines(c(x[perm.i[1,i]], x[perm.i[2,i]]), c(y[perm.i[1,i]], y[perm.i[2,i]]), lwd=2, col='green')
		
		hist(slopes, breaks=10)
	}
	
	for(i in 1:20){
		plot(x, y)
		abline(0, median(slopes), col='green', lwd=2)
		abline(0, 1, col='black', lwd=2)
		
		hist(slopes, breaks=10)
	}
}

saveGIF(to.plot(), 'd:/animated.sens.gif', ani.interval=0.5, ani.width=960)

##Seasonal
n = 4*12+1
season = rep(1:12, length.out=n)
all.x = 2*pi*seq(0, 4, length.out=n)
all.y = sin(all.x) + rnorm(length(all.x), sd=0.2) +0.01*all.x
plot(all.x, all.y, type='o')

to.plot = function(){
	
	par(mfrow=c(1,2))
	all.slopes = c()
	
	for(j in 1:12){
		x = all.x[season==j]
		y = all.y[season==j]
		
		perm.i = combn(1:length(x), 2)
		
		for(i in 1:ncol(perm.i)){
			
			
			all.slopes = c(all.slopes, (y[perm.i[1,i]]-y[perm.i[2,i]])/(x[perm.i[1,i]] - x[perm.i[2,i]]))
			plot(all.x, all.y, type='o')
			lines(c(x[perm.i[1,i]], x[perm.i[2,i]]), c(y[perm.i[1,i]], y[perm.i[2,i]]), lwd=2, col='green')
			
			hist(all.slopes, breaks=30)
		}
		
	}
	
	cat(median(all.slopes))
	
	for(i in 1:20){
		plot(all.x, all.y, type='o')
		abline(0, median(all.slopes), col='green', lwd=2)
		abline(0, 0.01, col='black', lwd=2)
		lines(all.x, predict(lm(all.y~all.x)), col='red',lwd=2)
		
		hist(all.slopes, breaks=30)
	}
}


saveGIF(to.plot(), 'd:/animated.seasonal.sens.gif', ani.interval=0.1, ani.width=960)

#save final fig

all.slopes = c()

for(j in 1:12){
	x = all.x[season==j]
	y = all.y[season==j]
	
	perm.i = combn(1:length(x), 2)
	
	for(i in 1:ncol(perm.i)){
		all.slopes = c(all.slopes, (y[perm.i[1,i]]-y[perm.i[2,i]])/(x[perm.i[1,i]] - x[perm.i[2,i]]))
	}
	
}

png('d:/animated.seasonal.final.png', width=960, height=480)
par(mfrow=c(1,2))
plot(all.x, all.y, type='o')
abline(0, median(all.slopes), col='green', lwd=2)
abline(0, 0.01, col='black', lwd=2)
lines(all.x, predict(lm(all.y~all.x)), col='red',lwd=2)

hist(all.slopes, breaks=30)
dev.off()





