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
	## we need some help from our friend the linear model (regression)
lm.res <- lm(TW_CAD~LCBO_CAD,data= PRICE.DATA)
	## equivalence
cor(PRICE.DATA$TW_CAD,predict(lm.res))
cor(PRICE.DATA$TW_CAD,lm.res$fitted.values)


### Rule 0: Visualize your data, espeically before you do anything.
dev.new()
plot(PRICE.DATA,pch=20,asp=1,main=paste0("American Beer in CAD (355ml serving)\n r = ", round(cor.test.res$estimate,digits=3)),xlab="CAD: LCBO",ylab="CAD: TW",col=ifelse(cost.ratio>1,"mediumorchid4","olivedrab4"),cex=2)
text(PRICE.DATA,labels=rownames(PRICE.DATA),pos=3,cex=.65,col=ifelse(cost.ratio>1,"mediumorchid4","olivedrab4"))
abline(lm.res,col="blue",lty=2,lwd=1.75)

### Visualize the prediction of the full data.
dev.new()
plot(PRICE.DATA,pch=20,asp=1,main=paste0("American Beer in CAD (355ml serving)\n r = ", round(cor.test.res$estimate,digits=3)),xlab="CAD: LCBO",ylab="CAD: TW",col="mediumorchid4",cex=2,ylim=c(min(c(PRICE.DATA[,2],predict(lm.res))),max(c(PRICE.DATA[,2],predict(lm.res)))))
abline(lm.res,col="blue",lty=2,lwd=1.75)
points(cbind(PRICE.DATA[,1],predict(lm.res)),col="seagreen3",pch=20,cex=1.5)
for(i in 1:nrow(PRICE.DATA)){
	x1 <- x0 <- PRICE.DATA[i,1]
	y0 <- PRICE.DATA[i,2]
	y1 <- predict(lm.res)[i]
	segments(x0,y0,x1,y1,lty=2,col="seagreen3")
}


#############
#########
#####
#				What does split-half look like?
#####
#########
#############

set.seed(13)		## set a seed for the random number generator so we can recreate the result exactly

PD.sh.1.indices <- sort(sample(nrow(PRICE.DATA),round(nrow(PRICE.DATA)*.5),replace=F))
PD.sh.2.indices <- setdiff(1:nrow(PRICE.DATA),PD.sh.1.indices)

PD.sh.1.lm <- lm(TW_CAD~LCBO_CAD,data=PRICE.DATA,subset=PD.sh.1.indices)
PD.sh.2.lm <- lm(TW_CAD~LCBO_CAD,data=PRICE.DATA,subset=PD.sh.2.indices)	
	
PD.2.from.1 <- predict(PD.sh.1.lm, PRICE.DATA[PD.sh.2.indices,])
PD.1.from.2 <- predict(PD.sh.2.lm, PRICE.DATA[PD.sh.1.indices,])	


dev.new()
plot(PRICE.DATA,pch=20,asp=1,main="American Beer in CAD (355ml serving)\n ",xlab="CAD: LCBO",ylab="CAD: TW",col="grey80",cex=2,ylim=c(min(c(PRICE.DATA[,2],PD.2.from.1)),max(c(PRICE.DATA[,2],PD.2.from.1))))
#text(PRICE.DATA,labels=rownames(PRICE.DATA),pos=3,cex=.65,col="grey80")
points(PRICE.DATA[PD.sh.1.indices,],col="mediumorchid4",pch=20,cex=2)
abline(lm.res,col="blue",lty=2,lwd=1.75)
abline(PD.sh.1.lm,col="mediumorchid4",lty=1,lwd=2)
points(cbind(PRICE.DATA[PD.sh.1.indices,1],predict(PD.sh.1.lm)),col="mediumorchid4",pch=20,cex=1)
for(i in 1:length(PD.sh.1.indices)){
	x1 <- x0 <- PRICE.DATA[PD.sh.1.indices[i],1]
	y0 <- PRICE.DATA[PD.sh.1.indices[i],2]
	y1 <- predict(PD.sh.1.lm)[i]
	segments(x0,y0,x1,y1,lty=2,col="mediumorchid4")
}
points(cbind(PRICE.DATA[PD.sh.2.indices,1],PD.2.from.1),pch=20,cex=1.5,col="seagreen3")
for(i in 1:length(PD.sh.2.indices)){
	x1 <- x0 <- PRICE.DATA[PD.sh.2.indices[i],1]
	y0 <- PRICE.DATA[PD.sh.2.indices[i],2]
	y1 <- PD.2.from.1[i]
	segments(x0,y0,x1,y1,lty=2,col="seagreen4")
}


sh.predicted <- c(PD.2.from.1,PD.1.from.2)[rownames(PRICE.DATA)]	
	## similarity.
split.cor <- cor(sh.predicted,PRICE.DATA$TW_CAD)
	## prediction error.
split.mpse <- mean((PRICE.DATA$TW_CAD - sh.predicted) ^ 2)


#############
#########
#####
#				Turn split-half into a test
#####
#########
#############


iters <- 1000
split.mat <- matrix(NA,iters,2)
	colnames(split.mat) <- c("cor","mpse")	
for(i in 1:iters){		
	
	PD.sh.1.indices <- sort(sample(nrow(PRICE.DATA),round(nrow(PRICE.DATA)*.5),replace=F))
	PD.sh.2.indices <- setdiff(1:nrow(PRICE.DATA),PD.sh.1.indices)
			
	PD.sh.1.lm <- lm(TW_CAD~LCBO_CAD,data=PRICE.DATA,subset=PD.sh.1.indices)
	PD.sh.2.lm <- lm(TW_CAD~LCBO_CAD,data=PRICE.DATA,subset=PD.sh.2.indices)	
	
	PD.2.from.1 <- predict(PD.sh.1.lm, PRICE.DATA[PD.sh.2.indices,])
	PD.1.from.2 <- predict(PD.sh.2.lm, PRICE.DATA[PD.sh.1.indices,])	
	
	sh.predicted <- c(PD.2.from.1,PD.1.from.2)[rownames(PRICE.DATA)]	
		## similarity
	split.cor <- cor(sh.predicted,PRICE.DATA$TW_CAD)
		## prediction error.
	split.mpse <- mean((PRICE.DATA$TW_CAD - sh.predicted) ^ 2)
	
	split.mat[i,1] <- split.cor
	split.mat[i,2] <- split.mpse	
	
}

pred.vals <- vector("numeric",nrow(PRICE.DATA))
for(i in 1:nrow(PRICE.DATA)){
	pred.vals[i] <- predict(lm(TW_CAD~LCBO_CAD,data=PRICE.DATA[-i,]),PRICE.DATA[i,])
}
loo.cor <- cor(PRICE.DATA$TW_CAD,pred.vals)
loo.mpse <- mean((PRICE.DATA$TW_CAD - pred.vals) ^ 2)

plot(split.mat,pch=20,col="black")
points(loo.cor, loo.mpse,col="red",pch=20)

dev.new()
boxplot(split.mat[,1])
dev.new()
boxplot(split.mat[,2])


