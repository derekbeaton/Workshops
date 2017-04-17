rm(list=ls())
gc()

library(ExPosition)
# library(ez)
# library(psych)

	## load data for this example
load('../Data/CAD.ABV.RATING.rda')

	## split the data into a DATA variable and a DESIGN (which tells us where the beers are from)
DATA <- CAD.ABV.RATING[,1:3]
DESIGN <- CAD.ABV.RATING $REGION
	
	## Standard PCA
pca.res <- epPCA(DATA,scale=T,DESIGN= DESIGN,graphs=F)

### Rule 0: Visualize your data, espeically before you do anything.
	### Scree plot
prettyScree(pca.res$ExPosition.Data$eigs)
	### Component scores for rows and columns
epGraphs(pca.res,contributionPlots=F,correlationPlotter=F)


## TODO
#############
#########
#####
#				Example of split-half
#####
#########
#############




#############
#########
#####
#				Turn split-half into a test
#####
#########
#############

	
iters <- 1000
	ortho.dists_fi <- ortho.dists_p <- matrix(NA,nrow(DATA),iters)
		rownames(ortho.dists_fi) <- rownames(ortho.dists_p) <- rownames(DATA)
	sh2.cors_fi <- sh1.cors_fi <- matrix(NA,iters,3)	
	sh2.fj_dist.cors <- sh1.fj_dist.cors <- sh2.p_dist.cors <- sh1.p_dist.cors <- vector("numeric",iters)
for(i in 1:iters){
	
	pca.sh1 <- sort(sample(1:nrow(DATA),nrow(DATA)/2))
	pca.sh2 <- setdiff(1:nrow(DATA),pca.sh1)
	
	pca.res_sh1 <- epPCA(DATA[pca.sh1,1:3],scale=T,graphs=F)
	pca.sh2.from.sh1 <- supplementaryRows(DATA[pca.sh2,1:3],pca.res_sh1)
	sh2.pred.p <- (pca.sh2.from.sh1$fii %*% diag(1/pca.res_sh1$ExPosition.Data$pdq$Dv))		
			
	pca.res_sh2 <- epPCA(DATA[pca.sh2,1:3],scale=T,graphs=F)
	pca.sh1.from.sh2 <- supplementaryRows(DATA[pca.sh1,1:3],pca.res_sh2)
	sh1.pred.p <- (pca.sh1.from.sh2$fii %*% diag(1/pca.res_sh2$ExPosition.Data$pdq$Dv))

	## similarities
	sh1.cors_fi[i,] <- diag(cor(pca.res_sh1$ExPosition.Data$fi,pca.sh1.from.sh2$fii)^2)
	sh2.cors_fi[i,] <- diag(cor(pca.res_sh2$ExPosition.Data$fi,pca.sh2.from.sh1$fii)^2)
	
	sh1.fj_dist.cors[i] <- cor(rowSums(pca.res_sh1$ExPosition.Data$fi^2),rowSums(pca.sh1.from.sh2$fii^2))
	sh1.p_dist.cors[i] <- cor(rowSums(pca.res_sh1$ExPosition.Data$pdq$p^2),rowSums(sh1.pred.p^2))
	
	sh2.fj_dist.cors[i] <- cor(rowSums(pca.res_sh2$ExPosition.Data$fi^2),rowSums(pca.sh2.from.sh1$fii^2))
	sh2.p_dist.cors[i] <- cor(rowSums(pca.res_sh2$ExPosition.Data$pdq$p^2),rowSums(sh2.pred.p^2))
		
	## prediction errors		
	one.minus.pred_p <- (pca.res_sh1$ExPosition.Data$pdq$p - sh1.pred.p)^2
	two.minus.pred_p <- (pca.res_sh2$ExPosition.Data$pdq$p - sh2.pred.p)^2
	preds_p <- rbind(one.minus.pred_p, two.minus.pred_p)[rownames(ortho.dists_p),]
	ortho.dists_p[,i] <- rowSums(preds_p)
			
	one.minus.pred_fi <- (pca.res_sh1$ExPosition.Data$fi - pca.sh1.from.sh2$fii)^2
	two.minus.pred_fi <- (pca.res_sh2$ExPosition.Data$fi - pca.sh2.from.sh1$fii)^2
	preds_fi <- rbind(one.minus.pred_fi, two.minus.pred_fi)[rownames(ortho.dists_fi),]	
	ortho.dists_fi[,i] <- rowSums(preds_fi)	
	
	if(i%%100==0){
		print(i)
	}
		
}
PRESS_p <- colSums(ortho.dists_p)
PRESS_fi <- colSums(ortho.dists_fi)


### Just a few visualizers for now.

colnames(sh1.cors_fi) <- paste0("Comp. ",1:3)
dev.new()
boxplot(sh1.cors_fi,main="S-H Component Scores",col=c("red","green","blue"), ylab="Correlation values",xlab="Components")

dev.new()
boxplot(rbind(cbind(sh1.fj_dist.cors,sh1.p_dist.cors),cbind(sh2.fj_dist.cors,sh2.p_dist.cors)),col=c("mediumorchid4","olivedrab4"),main="Correlation: S-H Score and Mahalanobis Distances")

dev.new()
boxplot(cbind(PRESS_fi,PRESS_p),col=c("mediumorchid4","olivedrab4"),main="Correlation: S-H PRESS Scores and Vectors")


rownames(ortho.dists_fi) <- rownames(ortho.dists_p) <- abbreviate(rownames(ortho.dists_fi),2)
dev.new()
boxplot(t(ortho.dists_p),main="S-H Orthogonal Distance: Singular Vectors")

dev.new()
boxplot(t(ortho.dists_fi),main="S-H Orthogonal Distance: Component Scores")
