
## part 0: illustration of (some) cleaning with PCAs and PLS.

library(R.matlab)
library(oro.nifti)
library(TInPosition)


## we will want some MARINeR functions.
source('https://github.com/derekbeaton/MARINeR/blob/master/MARINeR/R/degree.detrend.R?raw=true')
source('https://github.com/derekbeaton/MARINeR/blob/master/MARINeR/R/percent.change.R?raw=true')


sub09.data <- readNIfTI('../Data/sub-09_task-onebacktask_run-01_bold_MNI.nii')
sub09.design <- read.csv('../Data/ds107_run-01_TR_DESIGN.csv',header=F)
sub09.onsets <- read.table('../Data/sub-09_run-01_onsets.txt',header=F,sep=" ")
sub09.mask <- readNIfTI('../Data/sub09_aal_4mm.nii')
sub09.mask_indices <- which(sub09.mask@.Data==1)

## just to tell us about the data.
print(sub09.data)




















## get the data into a vectorized format but masked out.

sub09.matrix <- t(apply(sub09.data@.Data,4,function(x){  c(x[sub09.mask_indices])  }  ))

## let's unpack the above line...
	## sub09.data@.Data is 4 dimensionsal: 4mm x 4mm x 4mm x 3sec
	## we apply a function over the fourth dimension
		### so that in the function we have a 4mm x 4mm x 4mm array
		### the function we apply is a concatenation (vectorize the cube) but only for specific indices (sub09.mask_indices).


	## create a purple-green gradient to look at each TR.
color.ramp <- colorRampPalette(c("mediumorchid3","seagreen3"))
row.colors <- color.ramp(nrow(sub09.matrix))








### CLEANING SELECTIONS AND PCA

# PCA on the data as they are
pca_asis <- epPCA(sub09.matrix,DESIGN=sub09.design,make_design_nominal=T,scale=F,graphs=F)
prettyPlot(pca_asis$ExPosition.Data$fi,col=pca_asis$Plotting.Data$fi.col,main="By block design")
prettyPlot(pca_asis$ExPosition.Data$fi,col= row.colors,main="By TR")
	
rownames(pca_asis$ExPosition.Data$fi) <- 1:nrow(pca_asis$ExPosition.Data$fi)	
prettyPlot(pca_asis$ExPosition.Data$fi,col= row.colors,main="By TR")
	## let's make this one *much* clearer
	points(pca_asis$ExPosition.Data$fi,type="l")
	






# PCA on detrend to first degree (linear)
sub09.matrix_detrend_deg1 <- degree.detrend(sub09.matrix,1)
pca_detrend1 <- epPCA(sub09.matrix_detrend_deg1,DESIGN=sub09.design,make_design_nominal=T,scale=F,graphs=F)
prettyPlot(pca_detrend1$ExPosition.Data$fi,col= pca_detrend1$Plotting.Data$fi.col,main="By block design")
prettyPlot(pca_detrend1$ExPosition.Data$fi,col= row.colors,main="By TR")

rownames(pca_detrend1$ExPosition.Data$fi) <- 1:nrow(pca_detrend1$ExPosition.Data$fi)	
prettyPlot(pca_detrend1$ExPosition.Data$fi,col= row.colors,main="By TR")
	## let's make this one *much* clearer
	points(pca_detrend1$ExPosition.Data$fi,type="l")


# PCA on detrend to second degree (linear + quadratic)
sub09.matrix_detrend_deg2 <- degree.detrend(sub09.matrix,2)
pca_detrend2 <- epPCA(sub09.matrix_detrend_deg2,DESIGN=sub09.design,make_design_nominal=T,scale=F,graphs=F)
prettyPlot(pca_detrend2$ExPosition.Data$fi,col= pca_detrend2$Plotting.Data$fi.col,main="By block design")
prettyPlot(pca_detrend2$ExPosition.Data$fi,col= row.colors,main="By TR")


# PCA on detrend to second degree (linear + quadratic + cubic)
sub09.matrix_detrend_deg3 <- degree.detrend(sub09.matrix,3)
pca_detrend3 <- epPCA(sub09.matrix_detrend_deg3,DESIGN=sub09.design,make_design_nominal=T,scale=F,graphs=F)
prettyPlot(pca_detrend3$ExPosition.Data$fi,col= pca_detrend2$Plotting.Data$fi.col,main="By block design")
prettyPlot(pca_detrend3$ExPosition.Data$fi,col= row.colors,main="By TR")



# PCA on detrend to second degree (linear + quadratic) with row normalization (i.e., z-score each row; which are TRs)
sub09.matrix_detrend_deg2_rownormed <- rowNorms(sub09.matrix_detrend_deg2,type="z")
pca_normed <- epPCA(sub09.matrix_detrend_deg2_rownormed,DESIGN=sub09.design,make_design_nominal=T,scale=F,graphs=F)
prettyPlot(pca_normed$ExPosition.Data$fi,col= pca_normed$Plotting.Data$fi.col,main="By block design")
prettyPlot(pca_normed$ExPosition.Data$fi,col= row.colors,main="By TR")



# Compute percentage change from reference TRs for block design; gives back each (useable) TR and block averaged (akin to PLSGui)
sub09.perc.change_info <- percent.change(sub09.matrix,sub09.onsets)

# PCA on percent signal change for all (usable) TRs
perc.change.pca <- epPCA(sub09.perc.change_info$perc.change.data,scale=F,DESIGN=sub09.perc.change_info$event.design,make_design_nominal=T,graphs=F)
prettyPlot(perc.change.pca$ExPosition.Data$fi,col=perc.change.pca$Plotting.Data$fi.col)


# PCA on percent signal change for all block averages
block.pca <- epPCA(sub09.perc.change_info$ave.perc.change.data,scale=F,DESIGN=sub09.perc.change_info$ave.row.design,make_design_nominal=T,graphs=F)
prettyPlot(block.pca$ExPosition.Data$fi,col= block.pca$Plotting.Data$fi.col)




### PLSs (specifically, barycentric discriminant analysis or mean centered PLS)

## with the full TR data (detrended, row normalized) we have to drop TRs that do not have any experimental data.
sub09.matrix_detrend_deg2_rownormed_dropped <- sub09.matrix_detrend_deg2_rownormed[-c(which(sub09.design=="drop")),]
sub09.design_dropped <- sub09.design[-c(which(sub09.design=="drop")),]


	### PLS on detrend (degree=2), row-normed, TR-dropped data.
pls.res <- tepBADA(sub09.matrix_detrend_deg2_rownormed_dropped,DESIGN= sub09.design_dropped,make_design_nominal=T,scale=F,graphs=F)
prettyScree(pls.res$TExPosition.Data$eigs)
prettyPlot(pls.res$TExPosition.Data$fi)

	### PLS on % signal change for all (usable) TRs
pls.perc.change <- tepBADA(sub09.perc.change_info$perc.change.data,scale=F,DESIGN=sub09.perc.change_info$event.design,make_design_nominal=T,graphs=F)
prettyScree(pls.perc.change$TExPosition.Data$eigs)
prettyPlot(pls.perc.change$TExPosition.Data$fi)

	### PLS on % signal change for block averages.
pls.block <- tepBADA(sub09.perc.change_info$ave.perc.change.data,scale=F,DESIGN=sub09.perc.change_info$ave.row.design,make_design_nominal=T,graphs=F)
prettyScree(pls.block$TExPosition.Data$eigs)
prettyPlot(pls.block$TExPosition.Data$fi)



