rm(list=ls())
gc()

# library(ExPosition)
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


#############
#########
#####
#				What does a permutation look like?
#####
#########
#############

set.seed(13)		## set a seed for the random number generator so we can recreate the result exactly


perm.aov.1 <- transform(REGION.STYLE.RATING,RATEBEER_StyleRating=sample(RATEBEER_StyleRating))
perm_anova_1 = ezANOVA(
    data = perm.aov.1
    , dv = RATEBEER_StyleRating
    , wid = BEER.NAME
    , between = c(REGION,PALE.v.NOT)
    , type = 3
)

dev.new()
interaction.plot(perm.aov.1$REGION, perm.aov.1$PALE.v.NOT, perm.aov.1$RATEBEER_StyleRating,type="b",xlab="REGION",ylab="STYLE RATINGS",trace.label="",pch=c(20,22),cex=2,lwd=2,col=c("mediumorchid4","darkseagreen"),main="PERMUTED: 2x2 ANOVA")



perm.aov.2 <- transform(REGION.STYLE.RATING,RATEBEER_StyleRating=sample(RATEBEER_StyleRating))
perm_anova_2 = ezANOVA(
    data = perm.aov.2
    , dv = RATEBEER_StyleRating
    , wid = BEER.NAME
    , between = c(REGION,PALE.v.NOT)
    , type = 3
)

dev.new()
interaction.plot(perm.aov.2$REGION, perm.aov.2$PALE.v.NOT, perm.aov.2$RATEBEER_StyleRating,type="b",xlab="REGION",ylab="STYLE RATINGS",trace.label="",pch=c(20,22),cex=2,lwd=2,col=c("mediumorchid4","darkseagreen"),main="PERMUTED: 2x2 ANOVA")


#############
#########
#####
#				Turn permutation into a test
#####
#########
#############

## Permute *just* the dependent variable so that it is with the wrong set of factors. Save the F-values.
iters <- 1000
	## real iterations to do
	#iters <- max(round(1/ratings_anova$ANOVA$p))+1
perm.fs <- matrix(NA,iters,3)
	colnames(perm.fs) <- c("REGION","PALE.v.NOT","REGION:PALE.v.NOT")
for(i in 1:iters){
	
	perm.aov <- transform(REGION.STYLE.RATING,RATEBEER_StyleRating=sample(RATEBEER_StyleRating))

	perm_aov = suppressWarnings(ezANOVA(
    data = perm.aov
    , dv = RATEBEER_StyleRating
    , wid = BEER.NAME
    , between = c(REGION,PALE.v.NOT)
    , type = 3
	))
	
	### This takes a little bit longer than correlation, so we will tell ourselves on how many iterations it has been
	perm.fs[i,] <- perm_aov$ANOVA$F
	if(i%%100==0){
		print(i)
	}
}

## Visualize all the permuted F distributions with cut-offs and observed statistics
cut.off <- sort(perm.fs[,1])[round(nrow(perm.fs)*.95)]
dev.new()
hist(perm.fs[,1],breaks=50,xlim=c(-.1,max(ratings_anova$ANOVA$F,c(perm.fs))*1.1),border="white",col="mediumorchid4",main="Permutation F: REGION",xlab="Permuted Fs")
abline(v= cut.off,col="firebrick3",lty=2,lwd=1.75)
abline(v= ratings_anova$ANOVA$F[1],col="olivedrab3",lty=1,lwd=1.75)

cut.off <- sort(perm.fs[,2])[round(nrow(perm.fs)*.95)]
dev.new()
hist(perm.fs[,2],breaks=50,xlim=c(-.1,max(ratings_anova$ANOVA$F,c(perm.fs))*1.1),border="white",col="mediumorchid4",main="Permutation F: PALE.v.NOT",xlab="Permuted Fs")
abline(v= cut.off,col="firebrick3",lty=2,lwd=1.75)
abline(v= ratings_anova$ANOVA$F[2],col="olivedrab3",lty=1,lwd=1.75)

cut.off <- sort(perm.fs[,3])[round(nrow(perm.fs)*.95)]
dev.new()
hist(perm.fs[,3],breaks=50,xlim=c(-.1,max(ratings_anova$ANOVA$F,c(perm.fs))*1.1),border="white",col="mediumorchid4",main="Permutation F: INTERACTION",xlab="Permuted Fs")
abline(v= cut.off,col="firebrick3",lty=2,lwd=1.75)
abline(v= ratings_anova$ANOVA$F[3],col="olivedrab3",lty=1,lwd=1.75)

## Finally, we compute the p-values
aov.ps <- colSums(perm.fs > matrix(ratings_anova$ANOVA$F,iters,3,byrow=T)) / iters
aov.ps.adj <- pmax(aov.ps,1/iters)
