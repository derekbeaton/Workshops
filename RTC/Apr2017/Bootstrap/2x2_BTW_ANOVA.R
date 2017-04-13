rm(list=ls())
gc()

library(InPosition)
library(ez)
# library(psych)

	## load data for this example
load('../Data/REGION.STYLE.RATING.rda')

	## parametric approach, with the ez package
ratings_anova = ezANOVA(
    data = REGION.STYLE.RATING
    , dv = RATEBEER_StyleRating
    , wid = BEER.NAME
    , between = c(REGION,PALE.v.NOT)
    , type = 3
)

### Rule 0: Visualize your data, espeically before you do anything.
dev.new()
interaction.plot(REGION.STYLE.RATING$REGION, REGION.STYLE.RATING$PALE.v.NOT, REGION.STYLE.RATING$RATEBEER_StyleRating,type="b",xlab="REGION",ylab="STYLE RATINGS",trace.label="",pch=c(20,22),cex=2,lwd=2,col=c("mediumorchid4","darkseagreen"),main="2x2 ANOVA")


## We will need a design-like matrix for bootstrap resampling.
RSR.design <- makeNominalData(as.matrix(apply(REGION.STYLE.RATING[,2:3],1,paste,collapse="_")))



#############
#########
#####
#				What does a bootstrap look like?
#####
#########
#############


set.seed(13)		## set a seed for the random number generator so we can recreate the result exactly

	## need a better name.
boot.aov.1 <- REGION.STYLE.RATING[sort(boot.samples(REGION.STYLE.RATING,RSR.design,constrained=T)),]
boot.aov.1$BEER.NAME <- as.factor(rownames(boot.aov.1))

boot_anova_1 = ezANOVA(
    data = droplevels(boot.aov.1)
    , dv = RATEBEER_StyleRating
    , wid = BEER.NAME
    , between = c(REGION,PALE.v.NOT)
    , type = 3
)

dev.new()
interaction.plot(boot.aov.1$REGION, boot.aov.1$PALE.v.NOT, boot.aov.1$RATEBEER_StyleRating,type="b",xlab="REGION",ylab="STYLE RATINGS",trace.label="",pch=c(20,22),cex=2,lwd=2,col=c("mediumorchid4","darkseagreen"))


boot.aov.2 <- REGION.STYLE.RATING[sort(boot.samples(REGION.STYLE.RATING,RSR.design,constrained=T)),]
boot.aov.2$BEER.NAME <- as.factor(rownames(boot.aov.2))

boot_anova_2 = ezANOVA(
    data = droplevels(boot.aov.2)
    , dv = RATEBEER_StyleRating
    , wid = BEER.NAME
    , between = c(REGION,PALE.v.NOT)
    , type = 3
    , return_aov = T
)

dev.new()
interaction.plot(boot.aov.2$REGION, boot.aov.2$PALE.v.NOT, boot.aov.2$RATEBEER_StyleRating,type="b",xlab="REGION",ylab="STYLE RATINGS",trace.label="",pch=c(20,22),cex=2,lwd=2,col=c("mediumorchid4","darkseagreen"))



	
iters <- 1000
	## real iterations to do
#iters <- max(round(1/ratings_anova$ANOVA$p))+1
boot.fs <- matrix(NA,iters,3)
	colnames(boot.fs) <- c("REGION","PALE.v.NOT","REGION:PALE.v.NOT")
	
boot.pale.means <- boot.region.means <- matrix(NA,iters,2)	
boot.cell.means <- array(NA,dim=c(2,2,iters))	
	
for(i in 1:iters){
	
	boot.aov <- REGION.STYLE.RATING[sort(boot.samples(REGION.STYLE.RATING,RSR.design,constrained=T)),]
	boot.aov$BEER.NAME <- as.factor(rownames(boot.aov))
	
	boot_anova = suppressWarnings(ezANOVA(
	    data = droplevels(boot.aov)
	    , dv = RATEBEER_StyleRating
	    , wid = BEER.NAME
	    , between = c(REGION,PALE.v.NOT)
	    , type = 3
    	, return_aov = T	    
	))
	
	
	## need to store the means, too
	boot.means <- model.tables(boot_anova$aov,type="means")
	boot.region.means[i,] <- boot.means$tables[[2]]
	boot.pale.means[i,] <- boot.means$tables[[3]]	
	boot.cell.means[,,i] <- boot.means$tables[[4]] 
	
	boot.fs[i,] <- boot_anova$ANOVA$F
	if(i%%100==0){
		print(i)
	}
}


## OK this visualization is a bit harder. Come back to it very soon.
