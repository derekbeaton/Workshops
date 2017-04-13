rm(list=ls())
gc()

library(InPosition)
library(ez)
# library(psych)

##split half by design function
sh.by.design <- function(DES){
	sh1 <- sort(unname(unlist(apply(RSR.design,2,function(x){y<-which(x==1); sample(y, ifelse(runif(1,min=0,max=1)>.5,floor(length(y)/2), ceiling(length(y)/2) ))}))))
	sh2 <- setdiff(1:nrow(DES),sh1)
	return(list(sh1=sh1,sh2=sh2))
}

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


# SH FOR ANOVA -- NOT SO BAD.
RSR.design <- makeNominalData(as.matrix(apply(REGION.STYLE.RATING[,2:3],1,paste,collapse="_")))


iters <- 1000	
split.mat_aov <- matrix(NA, iters,2)
	colnames(split.mat_aov) <- c("cor","mpse")	
for(i in 1:iters){		
	aov.splits <- sh.by.design(RSR.design)
	
	ratings_anova_SH1 = suppressWarnings(ezANOVA(
	    data = droplevels(REGION.STYLE.RATING[aov.splits$sh1,])
	    , dv = RATEBEER_StyleRating
	    , wid = BEER.NAME
	    , between = c(REGION,PALE.v.NOT)
	    , type = 3
	    , return_aov = T    
	))
	
	ratings_anova_SH2 = suppressWarnings(ezANOVA(
	    data = droplevels(REGION.STYLE.RATING[aov.splits$sh2,])
	    , dv = RATEBEER_StyleRating
	    , wid = BEER.NAME
	    , between = c(REGION,PALE.v.NOT)
	    , type = 3
	    , return_aov = T    
	))
	
	aov.2.from.1 <- predict(ratings_anova_SH1$aov,REGION.STYLE.RATING[aov.splits$sh2,])	
	aov.1.from.2 <- predict(ratings_anova_SH2$aov,REGION.STYLE.RATING[aov.splits$sh1,])
	
	sh.predicted_aov <- c(aov.2.from.1,aov.1.from.2)[rownames(REGION.STYLE.RATING)]	
		## repro/correlation?
	split.cor_aov <- cor(sh.predicted_aov,REGION.STYLE.RATING$RATEBEER_StyleRating)
		## prediction error.
	split.mpse_aov <- mean((REGION.STYLE.RATING$RATEBEER_StyleRating - sh.predicted_aov) ^ 2)	
	
	split.mat_aov[i,1] <- split.cor_aov
	split.mat_aov[i,2] <- split.mpse_aov	
	
}


## need visualizers