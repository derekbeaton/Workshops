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
fjj1 <- supplementaryCols(boot.pca.1, pca.res)


## Another bootstrap
boot.indices.2 <- sample(nrow(DATA),nrow(DATA),replace=T)
boot.pca.2 <- DATA[boot.indices.2,]
	
boot.pca.res.2 <- epPCA(boot.pca.2,scale=T,graphs=F)
prettyScree(boot.pca.res.2$ExPosition.Data$eigs)
epGraphs(boot.pca.res.2,contributionPlots=F,correlationPlotter=F)

### how variables differ when projected
fjj2 <- supplementaryCols(boot.pca.2, pca.res)



#############
#########
#####
#				Turn bootstrap into a test
#####
#########
#############

## Bootstrap observations and: (1) save the eigenvalues and (2) project the data and save the column factor scores

iters <- 1000
boot.comps <- matrix(NA,iters,length(pca.res$ExPosition.Data$eigs))
boot.vars <- array(NA,dim=c(nrow(pca.res$ExPosition.Data$fj),ncol(pca.res$ExPosition.Data$fj),iters))

for(i in 1:iters){
	
	boot.indices <- sample(nrow(DATA),nrow(DATA),replace=T)
	boot.pca <- DATA[boot.indices,]
	boot.pca.res <- epPCA(boot.pca,scale=T,graphs=F)
	
	boot.comps[i,1:length(boot.pca.res$ExPosition.Data$eigs)] <- boot.pca.res$ExPosition.Data$eigs
	
	if(i%%100==0){
		print(i)
	}
	
	## project and store
	boot.vars[,,i] <- supplementaryCols(boot.pca, pca.res)$fjj
	
}


## OK this visualization is a bit harder. Come back to it very soon.

