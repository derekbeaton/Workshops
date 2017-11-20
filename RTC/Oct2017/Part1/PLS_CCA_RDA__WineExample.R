rm(list=ls())
gc()


## we need some helper functions. 
source('https://github.com/derekbeaton/ExPosition-Family/blob/master/Code/R/Development/SlimPosition/R/tolerance.svd.R?raw=true')
source('https://github.com/derekbeaton/ExPosition-Family/blob/master/Code/R/Development/SlimPosition/R/power.rebuild_matrix.R?raw=true')


wine.dat <- read.csv('https://github.com/vguillemot/2Tables/blob/master/DataWineR4CCA.csv?raw=true')

objective.data <- as.matrix(wine.dat[,9:17])
subjective.data <- as.matrix(wine.dat[,4:8])
helper.data <- wine.dat[,1:3]


## we usually want to normalize the data matrices
objective.data_norm <- scale(objective.data)
subjective.data_norm <- scale(subjective.data)


### PLS, CCA, and RDA/RRR decompose a single matrix. The latent variable scores for the individuals are projected back onto the space. The optimization can be viewed as the SVD optimization or the maximization of (variance ranked, orthogonal) latent variables scores of the individuals.

####################
###############
##########
##### 				PLS; either matrix can be the left or right.
##########
###############
####################

R.pls <- t(objective.data_norm) %*% subjective.data_norm
	pls.res <- tolerance.svd(R.pls)
		## under the constraints of:
	t(pls.res$u) %*% pls.res$u # is identity matrix
	t(pls.res$v) %*% pls.res$v # is identity matrix

		## the latent variable scores are
	PLS.LV_objective <- objective.data_norm %*% pls.res$u
	PLS.LV_subjective <- subjective.data_norm %*% pls.res$v

		## with the optimization:
	diag(t(PLS.LV_objective) %*% PLS.LV_subjective) / pls.res$d



####################
###############
##########
##### 				CCA; either matrix can be the left or right.
##########
###############
####################

R.cca <- power.rebuild_matrix(crossprod(objective.data_norm),-1/2) %*% R.pls %*% power.rebuild_matrix(crossprod(subjective.data_norm),-1/2)
	cca.res <- tolerance.svd(R.cca)
		## under the constraints of:
	p <- power.rebuild_matrix(crossprod(objective.data_norm),-1/2) %*% cca.res$u	
	q <- power.rebuild_matrix(crossprod(subjective.data_norm),-1/2) %*% cca.res$v		

		## under the constraints of:	
	t(p) %*% crossprod(objective.data_norm) %*% p	# is identity matrix
	t(q) %*% crossprod(subjective.data_norm) %*% q	# is identity matrix
	
		## the latent variable scores are
	CCA.LV_objective <- objective.data_norm %*% p
	CCA.LV_subjective <- subjective.data_norm %*% q

		## with the optimization:
	diag(t(CCA.LV_objective) %*% CCA.LV_subjective) / cca.res$d


	## or just the built in CCA:
	base.cca <- cancor(objective.data_norm, subjective.data_norm)
		## the singular values are the optmization, i.e., "canonical correlations"
	cca.res$d / base.cca$cor
		## the coefficients are the generalized singular vectors:
	p / base.cca$xcoef[,1:5]
	q / base.cca$ycoef[,1:5]	
	

####################
###############
##########
##### 				RDA/RRR; cannot switch matrices. Objective data are privileged.
##########
###############
####################


R.rda_objective.privileged <- power.rebuild_matrix(crossprod(objective.data_norm),-1/2) %*% R.pls
	rda.res_objective.privileged <- tolerance.svd(R.rda_objective.privileged)
	rda.p_objective.privileged <- power.rebuild_matrix(crossprod(objective.data_norm),-1/2) %*% rda.res_objective.privileged$u
	
		## under the constraints of:	
	t(rda.p_objective.privileged) %*% crossprod(objective.data_norm) %*% rda.p_objective.privileged	# is identity matrix
	t(rda.res_objective.privileged$v) %*% rda.res_objective.privileged$v							# is identity matrix
	
	## the latent variable scores are
		## the latent variable scores are
	RDA.LV_objective_objective.privileged <- objective.data_norm %*% rda.p_objective.privileged
	RDA.LV_subjective_objective.privileged <- subjective.data_norm %*% rda.res_objective.privileged$v
	
			## with the optimization:
	diag(t(RDA.LV_objective_objective.privileged) %*% RDA.LV_subjective_objective.privileged) / rda.res_objective.privileged$d
	



####################
###############
##########
##### 				RDA/RRR; cannot switch matrices. Subjective data are privileged.
##########
###############
####################


R.rda_subjective.privileged <- power.rebuild_matrix(crossprod(subjective.data_norm),-1/2) %*% t(R.pls)
	rda.res_subjective.privileged <- tolerance.svd(R.rda_subjective.privileged)
	rda.p_subjective.privileged <- power.rebuild_matrix(crossprod(subjective.data_norm),-1/2) %*% rda.res_subjective.privileged$u
	
		## under the constraints of:	
	t(rda.p_subjective.privileged) %*% crossprod(subjective.data_norm) %*% rda.p_subjective.privileged
		# is identity matrix
	t(rda.res_subjective.privileged$v) %*% rda.res_subjective.privileged$v	# is identity matrix
	
	## the latent variable scores are
		## the latent variable scores are
	RDA.LV_objective_subjective.privileged <- subjective.data_norm %*% rda.p_subjective.privileged
	RDA.LV_subjective_subjective.privileged <- objective.data_norm %*% rda.res_subjective.privileged$v
	
			## with the optimization:
	diag(t(RDA.LV_objective_subjective.privileged) %*% RDA.LV_subjective_subjective.privileged) / rda.res_subjective.privileged$d
