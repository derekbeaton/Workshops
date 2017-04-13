rm(list=ls())
gc()

# library(ExPosition)
# library(ez)
# library(psych)

	## load data for this example
load('../Data/LCBO.TW.CAD.rda')

	## split the data into two separate variables for later use
PRICE.DATA <- LCBO.TW.CAD[,c("LCBO_CAD_serving","TW_CAD_serving")]
	colnames(PRICE.DATA) <- c("LCBO_CAD","TW_CAD")
cost.ratio <- LCBO.TW.CAD[,"LCBO.v.TW_CostRatio"]

	## parametric approach.
cor.test.res <- cor.test(PRICE.DATA[,1],PRICE.DATA[,2])

### Rule 0: Visualize your data, espeically before you do anything.
dev.new()
plot(PRICE.DATA,pch=20,asp=1,main="American Beer in CAD (355ml serving)\n",xlab="CAD: LCBO",ylab="CAD: TW",col="mediumorchid4",cex=2)
text(PRICE.DATA,labels=rownames(PRICE.DATA),pos=3,cex=.65,col="mediumorchid4")

dev.new()
plot(PRICE.DATA,pch=20,asp=1,main=paste0("American Beer in CAD (355ml serving)\n r = ", round(cor.test.res$estimate,digits=3)),xlab="CAD: LCBO",ylab="CAD: TW",col="mediumorchid4",cex=2)
text(PRICE.DATA,labels=rownames(PRICE.DATA),pos=3,cex=.65,col="mediumorchid4")

dev.new()
plot(PRICE.DATA,pch=20,asp=1,main=paste0("American Beer in CAD (355ml serving)\n r = ", round(cor.test.res$estimate,digits=3)),xlab="CAD: LCBO",ylab="CAD: TW",col=ifelse(cost.ratio>1,"mediumorchid4","olivedrab4"),cex=2)
text(PRICE.DATA,labels=rownames(PRICE.DATA),pos=3,cex=.65,col=ifelse(cost.ratio>1,"mediumorchid4","olivedrab4"))
legend("topleft",legend=c("Less in USA","Less in CAN"),pch=20,col=c("mediumorchid4","olivedrab4"),pt.cex=2)



#############
#########
#####
#				What does a permutation look like?
#####
#########
#############

set.seed(13)		## set a seed for the random number generator so we can recreate the result exactly

## A permutation
perm.1 <- cbind(PRICE.DATA[,1],sample(PRICE.DATA[,2]))
	rownames(perm.1) <- rownames(PRICE.DATA)
	colnames(perm.1) <- colnames(PRICE.DATA)	

dev.new()
plot(perm.1,pch=20,asp=1,main=paste0("PERMUTED American Beer in CAD (355ml serving)\n r = ", round(cor(perm.1)[2,1],digits=3)),xlab="CAD: LCBO",ylab="CAD: TW",col="mediumorchid4",cex=2)
text(perm.1,labels=rownames(perm.1),pos=3,cex=.65,col="mediumorchid4")
	
## Another permutation	
perm.2 <- cbind(PRICE.DATA[,1],sample(PRICE.DATA[,2]))
	rownames(perm.2) <- rownames(PRICE.DATA)
	colnames(perm.2) <- colnames(PRICE.DATA)		
	## save this out	
	
dev.new()
plot(perm.2,pch=20,asp=1,main=paste0("PERMUTED American Beer in CAD (355ml serving)\n r = ", round(cor(perm.2)[2,1],digits=3)),xlab="CAD: LCBO",ylab="CAD: TW",col="mediumorchid4",cex=2)
text(perm.2,labels=rownames(perm.2),pos=3,cex=.65,col="mediumorchid4")


#############
#########
#####
#				Turn permutation into a test
#####
#########
#############

## Permute a column to break the relationship. Save the r values in a vector.
iters <- 1000
## NOTE: The real number of iterations we *should* do is: (1/cor.test.res$p.value)+1
perm.rs <- vector("numeric",iters)
for(i in 1:iters){
	
	perm.rs[i] <- cor(PRICE.DATA[,1],sample(PRICE.DATA[,2]))
		
}

## First we visualize the distribution of permuted r values
dev.new()
hist(perm.rs,breaks=50,xlim=c(-1,1),border="white",col="mediumorchid4",main="Permutation Distribution",xlab="Permuted correlations")

## Second we show a one-tailed test, assuming the direction of the observed statistic
cut.off <- sort(perm.rs)[round(length(perm.rs)*.95)]
dev.new()
hist(perm.rs,breaks=50,xlim=c(-1,1),border="white",col="mediumorchid4",main="Permutation: One-tailed",xlab="Permuted correlations")
abline(v= cut.off,col="firebrick3",lty=2,lwd=1.75)
abline(v= cor.test.res$estimate,col="olivedrab3",lty=1,lwd=1.75)

## Next we visualize the cutoffs for a two-tailed test
cut.offs <- sort(perm.rs)[round(c(length(perm.rs)*.025,length(perm.rs)*.975))]
dev.new()
hist(perm.rs,breaks=50,xlim=c(-1,1),border="white",col="mediumorchid4",main="Permutation: Two-tailed",xlab="Permuted correlations")
abline(v= cut.offs[1],col="firebrick3",lty=2,lwd=1.75)
abline(v= cut.offs[2],col="firebrick3",lty=2,lwd=1.75)
abline(v= cor.test.res$estimate,col="olivedrab3",lty=1,lwd=1.75)

## Or we can just do a one-tail test of r-squared
cut.off <- sort(perm.rs^2)[round(length(perm.rs)*.95)]
dev.new()
hist(perm.rs^2,breaks=25,xlim=c(0,1),border="white",col="mediumorchid4",main="Permutation: One-tailed R-squared",xlab="Permuted r-squared")
abline(v= cut.off,col="firebrick3",lty=2,lwd=1.75)
abline(v= cor.test.res$estimate^2,col="olivedrab3",lty=1,lwd=1.75)


## Finally, we compute the p-values
## one tail r test
	one.tail.cor.p <- max(1/iters,sum(perm.rs > cor.test.res$estimate)/iters)
## "two tailed" via r-squared test
	two.tail.r2.p <- max(1/iters,sum(perm.rs^2 > cor.test.res$estimate^2)/iters)

