rm(list=ls())
gc()

library(ExPosition)
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
#				What does a permutation look like?
#####
#########
#############

set.seed(13)		## set a seed for the random number generator so we can recreate the result exactly

## A permutation
perm.pca.1 <- apply(DATA,2,sample)
	rownames(perm.pca.1) <- rownames(CAD.ABV.RATING)
	
perm.pca.res.1 <- epPCA(perm.pca.1,scale=T,DESIGN= DESIGN,graphs=F)
prettyScree(perm.pca.res.1$ExPosition.Data$eigs)
epGraphs(perm.pca.res.1,contributionPlots=F,correlationPlotter=F)

## Another permutation
perm.pca.2 <- apply(DATA,2,sample)
	rownames(perm.pca.2) <- rownames(CAD.ABV.RATING)
	
perm.pca.res.2 <- epPCA(perm.pca.2,scale=T,DESIGN= DESIGN,graphs=F)
prettyScree(perm.pca.res.2$ExPosition.Data$eigs)
epGraphs(perm.pca.res.2,contributionPlots=F,correlationPlotter=F)


#############
#########
#####
#				Turn permutation into a test
#####
#########
#############

## Permute *each* column to break the relationship. Save the eigenvalues.
pca.iters <- 1000
perm.comps <- matrix(NA,pca.iters,length(perm.pca.res.1$ExPosition.Data$eigs))

for(i in 1:pca.iters){
	
	perm.pca <- apply(DATA,2,sample)
	perm.pca.res <- epPCA(perm.pca,scale=T,DESIGN= DESIGN,graphs=F)
	
	perm.comps[i,1:length(perm.pca.res$ExPosition.Data$eigs)] <- perm.pca.res$ExPosition.Data$eigs
	
	if(i%%100==0){
		print(i)
	}
}


## Visualize the distribution of permuted eigenvalues with cut-offs
cut.off <- sort(perm.comps[,1])[round(nrow(perm.comps)*.95)]
dev.new()
hist(perm.comps[,1],breaks=50,xlim=c(min(pca.res$ExPosition.Data$eigs,c(perm.comps)),max(pca.res$ExPosition.Data$eigs,c(perm.comps))*1.1),border="white",col="mediumorchid4",main="Permutation Comp. 1",xlab="Permuted eigenvalues")
abline(v= cut.off,col="firebrick3",lty=2,lwd=1.75)
abline(v= pca.res$ExPosition.Data$eigs[1],col="olivedrab3",lty=1,lwd=1.75)

cut.off <- sort(perm.comps[,2])[round(nrow(perm.comps)*.95)]
dev.new()
hist(perm.comps[,2],breaks=50,xlim=c(min(pca.res$ExPosition.Data$eigs,c(perm.comps)),max(pca.res$ExPosition.Data$eigs,c(perm.comps))*1.1),border="white",col="mediumorchid4",main="Permutation Comp. 2",xlab="Permuted eigenvalues")
abline(v= cut.off,col="firebrick3",lty=2,lwd=1.75)
abline(v= pca.res$ExPosition.Data$eigs[2],col="olivedrab3",lty=1,lwd=1.75)

cut.off <- sort(perm.comps[,3])[round(nrow(perm.comps)*.95)]
dev.new()
hist(perm.comps[,3],breaks=50,xlim=c(min(pca.res$ExPosition.Data$eigs,c(perm.comps)),max(pca.res$ExPosition.Data$eigs,c(perm.comps))*1.1),border="white",col="mediumorchid4",main="Permutation Comp. 3",xlab="Permuted eigenvalues")
abline(v= cut.off,col="firebrick3",lty=2,lwd=1.75)
abline(v= pca.res$ExPosition.Data$eigs[3],col="olivedrab3",lty=1,lwd=1.75)

## Finally, we compute the p-values
pca.ps <- colSums(perm.comps > matrix(pca.res$ExPosition.Data$eigs,pca.iters,3,byrow=T)) / pca.iters
pca.ps.adj <- pmax(pca.ps,1/pca.iters)
