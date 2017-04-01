

#rm(list=ls()) #run this to clear the workspace.

#load the data.
data(words)
	#rear axle ratio
W <- words$data[,1]
	#weight
Y <- words$data[,2]


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


###Permutation
#a single example of a permutation resampling
perm.indices <- sample(1:length(W),length(W),FALSE)
W.perm <- W[perm.indices]
perm.cor <- cor(W.perm,Y)


##set up for the permutation
nperms <- 1000
r.perm.values <- matrix(0,nperms,1)
r.squared.perm.values <- matrix(0,nperms,1)
F.perm.values <- matrix(0,nperms,1)


for(i in 1:nperms){
	
	perm.indices <- sample(1:length(W),length(W),FALSE)
	W.perm <- W[perm.indices]
	r.perm.values[i,1] <- cor(W.perm,Y)
	
}
##some clean up
r.perm.values <- replace(r.perm.values,is.na(r.perm.values),0)
r.perm.values <- replace(r.perm.values,is.nan(r.perm.values),0)
r.perm.values <- replace(r.perm.values,is.infinite(r.perm.values),0)

r.squared.perm.values <- r.perm.values^2
F.perm.values <- ((r.squared.perm.values)/(1-r.squared.perm.values)) * (length(W) - 2)



dev.new()
h<-hist(r.perm.values,breaks=100,xlim=c(-1.1,1.1),col="purple")
abline(v=r.value,col="green",lwd=3)


dev.new()
hist(r.squared.perm.values,breaks=100,xlim=c(-0.1,1.1),col="purple")
abline(v=quantile(r.squared.perm.values,probs=c(0.975)),col="red",lwd=3)
abline(v=r.squared,col="green",lwd=3)


dev.new()
hist(F.perm.values,breaks=100,col="purple",main="F Permutation Distribution",xlab="")
abline(v=quantile(F.perm.values,probs=c(0.975)),col="red",lwd=3)
abline(v=F.value,col="green",lwd=3)
