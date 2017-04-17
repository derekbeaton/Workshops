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
    , return_aov = T    
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
    , return_aov = T    
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


## For something like ANOVA, it makes more sense to visualize the means (especially cell wise) than the F-values
	## though, anything can be saved and visualized from ANOVA (or any other technique)
	## for example, ez calls into R's linear model function and it is therefore possible to get coefficients
	
CIs_REGION <- apply(boot.region.means,2,function(x){ sort(x)[round(c(length(x)*.025,length(x)*.975))] })
CIs_PALE <- apply(boot.pale.means,2,function(x){ sort(x)[round(c(length(x)*.025,length(x)*.975))] })
CIs_INTERACTION <- apply(boot.cell.means,c(1,2),function(x){ sort(x)[round(c(length(x)*.025,length(x)*.975))] })
	
aov.means <- model.tables(ratings_anova$aov,type="means")
		
dev.new()
hist(boot.region.means[,1],breaks=30,border="white",col="mediumorchid2",main="REGION: Bootstrap Distribution",xlab="REGION: Bootstrapped means",xlim=c(min(boot.region.means),max(boot.region.means)))
abline(v=aov.means$tables$REGION[1],lwd=3,col="olivedrab4")
abline(v=CIs_REGION[,1],lwd=1.75,col="firebrick4",lty=2)
hist(boot.region.means[,2],breaks=30,border="white",col="mediumorchid4",add=T)
abline(v=aov.means$tables$REGION[2],lwd=3,col="olivedrab2")
abline(v=CIs_REGION[,2],lwd=1.75,col="firebrick2",lty=2)


dev.new()
hist(boot.pale.means[,1],breaks=30,border="white",col="mediumorchid2",main="PALE v NOT: Bootstrap Distribution",xlab="PALE v NOT: Bootstrapped means",xlim=c(min(boot.pale.means),max(boot.pale.means)))
abline(v=aov.means$tables$PALE.v.NOT[1],lwd=3,col="olivedrab4")
abline(v= CIs_PALE[,1],lwd=1.75,col="firebrick4",lty=2)
hist(boot.pale.means[,2],breaks=30,border="white",col="mediumorchid4",add=T)
abline(v=aov.means$tables$PALE.v.NOT[2],lwd=3,col="olivedrab2")
abline(v= CIs_PALE[,2],lwd=1.75,col="firebrick2",lty=2)


	## for this one it would be better to have density curves
dev.new()
hist(boot.cell.means[1,1,],breaks=25,border="white",col="mediumorchid1",main="INTERACTION: Bootstrap Distribution",xlab="INTERACTION: Bootstrapped means",xlim=c(min(boot.cell.means),max(boot.cell.means)))
hist(boot.cell.means[1,2,],breaks=25,border="white",col="mediumorchid2",add=T)
hist(boot.cell.means[2,1,],breaks=25,border="white",col="mediumorchid3",add=T)
hist(boot.cell.means[2,2,],breaks=25,border="white",col="mediumorchid4",add=T)




abline(v=aov.means$tables$PALE.v.NOT[1],lwd=3,col="olivedrab4")
abline(v= CIs_PALE[,1],lwd=1.75,col="firebrick4",lty=2)
hist(boot.pale.means[,2],breaks=30,border="white",col="mediumorchid4",add=T)
abline(v=aov.means$tables$PALE.v.NOT[2],lwd=3,col="olivedrab2")
abline(v= CIs_PALE[,2],lwd=1.75,col="firebrick2",lty=2)




