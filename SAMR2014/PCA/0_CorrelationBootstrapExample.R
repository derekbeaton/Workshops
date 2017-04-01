

#rm(list=ls()) #run this to clear the workspace.

#load the data.
data(mtcars)
	#rear axle ratio
W <- mtcars$drat
	#weight
Y <- mtcars$hp


##Visualize
#dev.new()
#plot(W,Y,col="green",cex=3,pch=20,asp=1)  ##doesn't make much sense to plot them in their units!
dev.new()
plot(scale(W),scale(Y),col="green",cex=3,pch=20,asp=1)


##Compute the r, r-squared, F, and p values.
r.value <- cor(W, Y)
r.squared <- r.value^2
F.value <- (r.squared/(1-r.squared)) * (length(W) - 2)
p.value <- 1-pf(F.value,1,length(W)-2)


###Bootstrap
#a single example of a bootstrap resampling
boot.indices <- sample(1:length(W),length(W),TRUE)
W.boot <- W[boot.indices]
Y.boot <- Y[boot.indices]
plot(scale(W.boot),scale(Y.boot),col="green",cex=3,pch=20,asp=1)
boot.cor <- cor(W.boot,Y.boot)


##set up for the bootstrap
nboots <- 1000
r.boot.values <- matrix(0,nboots,1)
r.squared.boot.values <- matrix(0,nboots,1)
F.boot.values <- matrix(0,nboots,1)

for(i in 1:nboots){
	
	boot.indices <- sample(1:length(W),length(W),TRUE)
	W.boot <- W[boot.indices]
	Y.boot <- Y[boot.indices]	
	r.boot.values[i,1] <- cor(W.boot,Y.boot)
	
}
##some clean up
r.boot.values <- replace(r.boot.values,is.na(r.boot.values),0)
r.boot.values <- replace(r.boot.values,is.nan(r.boot.values),0)
r.boot.values <- replace(r.boot.values,is.infinite(r.boot.values),0)

r.squared.boot.values <- r.boot.values^2
F.boot.values <- ((r.squared.boot.values)/(1-r.squared.boot.values)) * (length(W) - 2)


###show the distributions and confidence intervals of r, r-squared, and F.
dev.new()
h<-hist(r.boot.values,breaks=100,xlim=c(-1.1,1.1),col="purple")
abline(v=quantile(r.boot.values,probs=c(0.975)),col="red",lwd=3)
abline(v=quantile(r.boot.values,probs=c(0.025)),col="red",lwd=3)
abline(v=r.value,col="green",lwd=3)


dev.new()
hist(r.squared.boot.values,breaks=100,xlim=c(-0.1,1.1),col="purple")
abline(v=quantile(r.squared.boot.values,probs=c(0.975)),col="red",lwd=3)
abline(v=quantile(r.squared.boot.values,probs=c(0.025)),col="red",lwd=3)
abline(v=r.squared,col="green",lwd=3)


dev.new()
hist(F.boot.values,breaks=100,col="purple")
abline(v=quantile(F.boot.values,probs=c(0.975)),col="red",lwd=3)
abline(v=quantile(F.boot.values,probs=c(0.025)),col="red",lwd=3)
abline(v=F.value,col="green",lwd=3)
