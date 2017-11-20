## part 1: illustration of (some) cleaning with PCAs and PLS.
	## this is a simple cleaning pipeline -- preprocessing can be (and often is) much more elaborate than just a detrend and row norms.
		### or sometimes it's much simpler with something like % change!


library(R.matlab)
library(oro.nifti)
library(TInPosition)

## we will want some MARINeR & SlimPosition functions.
source('https://github.com/derekbeaton/MARINeR/blob/master/MARINeR/R/degree.detrend.R?raw=true')
source('https://github.com/derekbeaton/MARINeR/blob/master/MARINeR/R/percent.change.R?raw=true')
source('https://github.com/derekbeaton/ExPosition-Family/blob/master/Code/R/Development/SlimPosition/R/sp.latentvar_plot.R?raw=true')
source('https://github.com/derekbeaton/ExPosition-Family/blob/master/Code/R/Development/SlimPosition/R/sp.scree.R?raw=true')
source('https://github.com/derekbeaton/ExPosition-Family/blob/master/Code/R/Development/SlimPosition/R/sp.component_plot.R?raw=true')


empty.brain <- MNI_444 <- readNIfTI('../Data/MNI152_T1_4mm_brain.nii')
	empty.brain@.Data <- array(0,dim=c(dim(empty.brain@.Data)))
sub09.data <- readNIfTI('../Data/sub-09_task-onebacktask_run-01_bold_MNI.nii')
sub09.design <- read.csv('../Data/ds107_run-01_TR_DESIGN.csv',header=F)
sub09.mask <- readNIfTI('../Data/sub09_aal_4mm.nii')

sub09.mask_indices <- which(sub09.mask@.Data==1)

	## just to tell us about the data.
print(sub09.data)
image(sub09.data)


### let's save memory and just do all the preprocessing we want to do in one go.
sub09.matrix_detrend_deg2_rownormed_dropped <- 
			rowNorms(
				degree.detrend( 
					t(apply(sub09.data@.Data,4,function(x){  c(x[sub09.mask_indices])  }  )), 
				deg=2), 
			type = "z")[-c(which(sub09.design=="drop")),]
sub09.design_dropped <- sub09.design[-c(which(sub09.design=="drop")),]


### this is just fixed effects.
pls.res <- tepBADA(
					DATA = sub09.matrix_detrend_deg2_rownormed_dropped, 
					DESIGN = sub09.design_dropped, 
					make_design_nominal=T, 
					scale=F, 
					graphs=F
				)



iters <- 100
sv.perms <- matrix(NA,iters,length(pls.res$TExPosition.Data$eigs))
fi.boots <- array(NA,dim=c( nrow(pls.res$TExPosition.Data$fi),ncol(pls.res$TExPosition.Data$fi),iters) )
	rownames(fi.boots) <- rownames(pls.res$TExPosition.Data$fi)
fj.boots <- array(NA,dim=c( nrow(pls.res$TExPosition.Data$fj),ncol(pls.res$TExPosition.Data$fj),iters) )
	rownames(fj.boots) <- rownames(pls.res$TExPosition.Data$fj)
	
for(i in 1:iters){
	
	pls.perm <- tepBADA(sub09.matrix_detrend_deg2_rownormed_dropped, DESIGN = sub09.design_dropped[sample(length(sub09.design_dropped))], make_design_nominal=T, scale=F, graphs=F)
	sv.perms[i, 1:min(ncol(sv.perms), length(pls.perm$TExPosition.Data$eigs)) ] <- pls.perm$TExPosition.Data$eigs[1:min(ncol(sv.perms), length(pls.perm$TExPosition.Data$eigs))] 
	
	boot.samps <- boot.samples(sub09.matrix_detrend_deg2_rownormed_dropped,makeNominalData(as.matrix(sub09.design_dropped)))
	dat.boot <- sub09.matrix_detrend_deg2_rownormed_dropped[boot.samps,]
	des.boot <- makeNominalData(as.matrix(sub09.design_dropped))[boot.samps,]

	boot.R <- expo.scale(t(t(apply(des.boot, 1, "/", colSums(des.boot)))) %*% dat.boot,scale=F)
	fi.boots[,,i] <- boot.R %*% pls.res$TExPosition.Data$pdq$q
	fj.boots[,,i] <- t(boot.R) %*% pls.res$TExPosition.Data$pdq$p
}
fi.boots[which(is.na(fi.boots) | is.nan(fi.boots))] <- 0
fj.boots[which(is.na(fj.boots) | is.nan(fj.boots))] <- 0


component.p.vals <- pmax(colSums(matrix(pls.res$TExPosition.Data$eigs,iters,length(pls.res$TExPosition.Data$eigs),byrow=T) < sv.perms) / iters,1/iters)
fi.bsrs <- boot.ratio.test(fi.boots)
fj.bsrs <- boot.ratio.test(fj.boots)





## the big things we need:
dev.new()
par(mfrow=c(2,2))
	### scree plot
sp.scree(pls.res$TExPosition.Data$eigs)
	### bar of axis 1
h <- barplot(pls.res$TExPosition.Data$fi[,1],axes=F,axisnames=F,main="DESIGN: LV1",horiz=T,col=pls.res$Plotting.Data$fi.col)
abline(v=0)
legend("topright",legend=rownames(pls.res$TExPosition.Data$fi),pch=15,col=pls.res$Plotting.Data$fi.col,cex=.75,pt.cex=1,bty="n")
	### bar of axis 2
h <- barplot(pls.res$TExPosition.Data$fi[,2],axes=F,axisnames=F,main="DESIGN: LV2",col=pls.res$Plotting.Data$fi.col)	
abline(h=0)
legend("topleft",legend=rownames(pls.res$TExPosition.Data$fi),pch=15,col=pls.res$Plotting.Data$fi.col,cex=.75,pt.cex=1,bty="n")
	### component plot
sp.component_plot(pls.res$TExPosition.Data$fi,col=pls.res$Plotting.Data$fi.col)

	### latent variable plots
dev.new()	
par(mfrow=c(1,2))
sp.latentvar_plot(pls.res$TExPosition.Data,col=pls.res$Plotting.Data$fii.col)
sp.latentvar_plot(pls.res$TExPosition.Data,axis=2,col=pls.res$Plotting.Data$fii.col)




	## get a brain for C1 & C2.
c2.brain <- c1.brain <- empty.brain
c1.brain@.Data[sub09.mask_indices] <- (pls.res$TExPosition.Data$cj[,1] * sign(pls.res$TExPosition.Data$fj[,1])) * ncol(sub09.matrix_detrend_deg2_rownormed_dropped)
c2.brain@.Data[sub09.mask_indices] <- (pls.res$TExPosition.Data$cj[,2] * sign(pls.res$TExPosition.Data$fj[,2])) * ncol(sub09.matrix_detrend_deg2_rownormed_dropped)


## these are crude visualizations; one should definitely use the BSRs and a different visualizer (e.g., Mango or Papaya)

	## thresholded values for component 1 (positive side)
dev.new()
overlay(MNI_444,ifelse(c1.brain>1,c1.brain,NA),col.y="red")
	## Hot metal values for component 1 (positive side)
dev.new()
overlay(MNI_444,ifelse(c1.brain>1,c1.brain,NA))
	## Hot metal values for component 1 (negative side)
dev.new()
overlay(MNI_444,ifelse(c1.brain<(-1),abs(c1.brain),NA))

	## Hot metal values for component 2 (positive side)
dev.new()
overlay(MNI_444,ifelse(c2.brain>1,c2.brain,NA))

	## Hot metal values for component 2 (negative side)
dev.new()
overlay(MNI_444,ifelse(c2.brain<(-1),abs(c2.brain),NA))


writeNIfTI(c1.brain,file="Component1_ScaledContributions")
