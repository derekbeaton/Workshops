rm(list=ls())
gc()


## we need some helper functions. 
source('https://github.com/derekbeaton/ExPosition-Family/blob/master/Code/R/Development/SlimPosition/R/tolerance.svd.R?raw=true')
source('https://github.com/derekbeaton/ExPosition-Family/blob/master/Code/R/Development/SlimPosition/R/power.rebuild_matrix.R?raw=true')
source('https://github.com/derekbeaton/ExPosition-Family/blob/master/Code/R/Development/SlimPosition/R/invert.rebuild_matrix.R?raw=true')
source('https://github.com/derekbeaton/ExPosition-Family/blob/master/Code/R/Development/SlimPosition/R/isDiagonal.matrix.R?raw=true')
source('https://github.com/derekbeaton/ExPosition-Family/blob/master/Code/R/Development/SlimPosition/R/gsvd.R?raw=true')
source('https://github.com/derekbeaton/ExPosition-Family/blob/master/Code/R/Development/SlimPosition/R/expo.scale.R?raw=true')


wine.dat <- read.csv('https://github.com/vguillemot/2Tables/blob/master/DataWineR4CCA.csv?raw=true')
objective.data <- as.matrix(wine.dat[,9:17])
subjective.data <- as.matrix(wine.dat[,4:8])
helper.data <- wine.dat[,1:3]


simple.pls <- function(X,Y,x.center=T,x.scale="SS1",y.center=T,y.scale="SS1"){
	X <- expo.scale(X,center=x.center,scale=x.scale)
	Y <- expo.scale(Y,center=y.center,scale=y.scale)	
	gsvd.res <- gsvd(t(X) %*% Y)
	gsvd.res$LX <- X %*% gsvd.res$p
	gsvd.res$LY <- Y %*% gsvd.res$q		
	return(gsvd.res)			
}

	## not necessarily efficient but it is through the GSVD.
		### see ../Part1/PLS_CCA_RDA__WineExample.R for more efficient examples
	## also risky because I'm using pseudo-inverse (so be careful with rank deficient data)
simple.cca <- function(X,Y,x.center=T,x.scale="SS1",y.center=T,y.scale="SS1"){
	X <- expo.scale(X,center=x.center,scale=x.scale)
	Y <- expo.scale(Y,center=y.center,scale=y.scale)	
	
	gsvd.res <- gsvd(
		invert.rebuild_matrix(crossprod(X)) %*% t(X) %*% Y %*% invert.rebuild_matrix(crossprod(Y)),
		crossprod(X),
		crossprod(Y)
		)
		
	gsvd.res$LX <- X %*% gsvd.res$p
	gsvd.res$LY <- Y %*% gsvd.res$q		
	return(gsvd.res)			
}

	## not necessarily efficient but it is through the GSVD.
		### see ../Part1/PLS_CCA_RDA__WineExample.R for more efficient examples	
	## also risky because I'm using pseudo-inverse (so be careful with rank deficient data)	
simple.rrr <- function(X,Y,x.center=T,x.scale="SS1",y.center=T,y.scale="SS1"){
	X <- expo.scale(X,center=x.center,scale=x.scale)
	Y <- expo.scale(Y,center=y.center,scale=y.scale)	
	gsvd.res <- gsvd(
		invert.rebuild_matrix(crossprod(X)) %*% t(X) %*% Y,
		crossprod(X)
		)
	gsvd.res$LX <- X %*% gsvd.res$p
	gsvd.res$LY <- Y %*% gsvd.res$q		
	return(gsvd.res)			
}




pls.res <- simple.pls(objective.data, subjective.data)
	diag(t(pls.res$LX) %*% pls.res$LY) / pls.res$d
cca.res <- simple.cca(objective.data, subjective.data)
	diag(t(cca.res$LX) %*% cca.res$LY) / cca.res$d
rrr.res <- simple.rrr(objective.data, subjective.data)
	diag(t(cca.res$LX) %*% cca.res$LY) / cca.res$d


