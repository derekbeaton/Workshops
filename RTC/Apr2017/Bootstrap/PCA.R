rm(list=ls())
gc()

library(InPosition)
# library(ez)
# library(psych)

	## load data for this example
load('../Data/CAD.ABV.RATING.rda')

	## split the data into a DATA variable and a DESIGN (which tells us where the beers are from)
DATA <- CAD.ABV.RATING[,1:3]
DESIGN <- CAD.ABV.RATING$REGION
	
	## Standard PCA
pca.res <- epPCA(DATA,scale=T,DESIGN= DESIGN,graphs=F)

### Rule 0: Visualize your data, espeically before you do anything.
	### Scree plot
prettyScree(pca.res$ExPosition.Data$eigs)
	### Component scores for rows and columns
epGraphs(pca.res,contributionPlots=F,correlationPlotter=F)


#############
#########
#####
#				What does a bootstrap look like?
#####
#########
#############

set.seed(13)		## set a seed for the random number generator so we can recreate the result exactly

## A bootstrap
boot.indices.1 <- sample(nrow(DATA),nrow(DATA),replace=T)
boot.pca.1 <- DATA[boot.indices.1,]
	
boot.pca.res.1 <- epPCA(boot.pca.1,scale=T,graphs=F)
prettyScree(boot.pca.res.1$ExPosition.Data$eigs)
epGraphs(boot.pca.res.1,contributionPlots=F,correlationPlotter=F)

### how variables differ when projected
fjj1 <- boot.compute.fj(boot.pca.1, pca.res)


## Another bootstrap
boot.indices.2 <- sample(nrow(DATA),nrow(DATA),replace=T)
boot.pca.2 <- DATA[boot.indices.2,]
	
boot.pca.res.2 <- epPCA(boot.pca.2,scale=T,graphs=F)
prettyScree(boot.pca.res.2$ExPosition.Data$eigs)
epGraphs(boot.pca.res.2,contributionPlots=F,correlationPlotter=F)

### how variables differ when projected
fjj2 <- boot.compute.fj(boot.pca.2, pca.res)



#############
#########
#####
#				Turn bootstrap into a test
#####
#########
#############

## Bootstrap observations and: (1) save the eigenvalues and (2) project the data and save the column component scores
iters <- 1000
boot.comps <- matrix(NA,iters,length(pca.res$ExPosition.Data$eigs))
boot.vars <- array(NA,dim=c(nrow(pca.res$ExPosition.Data$fj),ncol(pca.res$ExPosition.Data$fj),iters))

for(i in 1:iters){
	
	boot.indices <- sample(nrow(DATA),nrow(DATA),replace=T)
	boot.pca <- DATA[boot.indices,]
	boot.pca.res <- epPCA(boot.pca,scale=T,graphs=F)
	
	boot.comps[i,1:length(boot.pca.res$ExPosition.Data$eigs)] <- boot.pca.res$ExPosition.Data$eigs
	
	## project and store
	boot.vars[,,i] <- boot.compute.fj(DATA, res = pca.res)
	
}


## Boot eigen visualization
boxplot(boot.comps,col=c("red","green","blue"),main="Bootstrapped and observed eigenvalues")
abline(h=pca.res$ExPosition.Data$eigs,lty=2,col=c("red","green","blue"),lwd=2)
points(pca.res$ExPosition.Data$eigs,type="l",lty=1,lwd=2,col="grey80")
points(pca.res$ExPosition.Data$eigs,bg=c("red","green","blue"),pch=21,cex=2)
axis(4,at=pca.res$ExPosition.Data$eigs,labels=round(pca.res$ExPosition.Data$eigs,digits=2))


## Just an example of the supplementary points from bootstrap
prettyPlot(pca.res$ExPosition.Data$fj,cex=1,col=c("red","green","blue"),contributions=pca.res$ExPosition.Data$cj,contributionCircles=T)
for(i in 1:100){
	prettyPlot(boot.vars[,,i],col=c("red","green","blue"),dev.new=F,axes=F,new.plot=F)
}
prettyPlot(pca.res$ExPosition.Data$fj,cex=1,col=c("red","green","blue"),contributions=pca.res$ExPosition.Data$cj,contributionCircles=T,dev.new=F,axes=F,new.plot=F)

## Peeled hulls for supplementary points from bootstrap
prettyPlot(pca.res$ExPosition.Data$fj,cex=1,col=c("red","green","blue"),contributions=pca.res$ExPosition.Data$cj,contributionCircles=T)
these.cols <- c("red","green","blue")
for(i in 1:3){
	peeledHull(t(boot.vars[i,1:2,]),col=these.cols[i],percentage=.95)
}

## Get bootstrap ratios
boot.ratio.test(boot.vars)

