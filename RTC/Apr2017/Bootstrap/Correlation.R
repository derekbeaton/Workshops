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




#############
#########
#####
#				What does a bootstrap look like?
#####
#########
#############

set.seed(13)		## set a seed for the random number generator so we can recreate the result exactly

boot.indices.1 <- sample(nrow(PRICE.DATA),nrow(PRICE.DATA),replace=T)
boot.1 <- PRICE.DATA[boot.indices.1,]	

dev.new()
plot(boot.1,pch=20,asp=1,main=paste0("BOOTSTRAPPED American Beer in CAD (355ml serving)\n r = ", round(cor(boot.1)[2,1],digits=3)),xlab="CAD: LCBO",ylab="CAD: TW",col="mediumorchid4",cex=2)
text(boot.1,labels=rownames(boot.1),pos=3,cex=.65,col="mediumorchid4")
	
	
	
boot.indices.2 <- sample(nrow(PRICE.DATA),nrow(PRICE.DATA),replace=T)
boot.2 <- PRICE.DATA[boot.indices.2,]	
	
dev.new()
plot(boot.2,pch=20,asp=1,main=paste0("BOOTSTRAPPED American Beer in CAD (355ml serving)\n r = ", round(cor(boot.2)[2,1],digits=3)),xlab="CAD: LCBO",ylab="CAD: TW",col="mediumorchid4",cex=2)
text(boot.2,labels=rownames(boot.2),pos=3,cex=.65,col="mediumorchid4")



#############
#########
#####
#				Turn bootstrap into a test
#####
#########
#############

iters <- 1000
boot.rs <- vector("numeric",iters)
for(i in 1:iters){
	
	boot.indices <- sample(nrow(PRICE.DATA),nrow(PRICE.DATA),replace=T)
	boot.rs[i] <- cor(PRICE.DATA[boot.indices,])[2,1]
	
	if( (i%%100)==0){
		print(i)
	}
		
}

dev.new()
hist(boot.rs,breaks=25,xlim=c(.5,1),border="white",col="mediumorchid4",main="Bootstrap Distribution",xlab="Bootstrapped correlations")

## First, we compute the confidence intervals (as percentiles)
CIs <- sort(boot.rs)[round(c(length(boot.rs)*.025,length(boot.rs)*.975))]

dev.new()
hist(boot.rs,breaks=25,xlim=c(-1,1),border="white",col="mediumorchid4",main="Bootstrap: CIs",xlab="Bootstrapped correlations")
abline(v= CIs[1],col="firebrick3",lty=2,lwd=1.75)
abline(v= CIs[2],col="firebrick3",lty=2,lwd=1.75)
abline(v= cor.test.res$estimate,col="olivedrab3",lty=1,lwd=1.75)

# Let's zoom in a bit.
dev.new()
hist(boot.rs,breaks=25,xlim=c(.5,1),border="white",col="mediumorchid4",main="Bootstrap: CIs",xlab="Bootstrapped correlations")
abline(v= CIs[1],col="firebrick3",lty=2,lwd=1.75)
abline(v= CIs[2],col="firebrick3",lty=2,lwd=1.75)
abline(v= cor.test.res$estimate,col="olivedrab3",lty=1,lwd=3)


