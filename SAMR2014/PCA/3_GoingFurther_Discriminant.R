### This file will be updated periodically between now (whenever you are seeing this)
### and April 2nd (the day before the workshop!)
#####
### If you download this file now, please be sure to check back for updates
### as the conference approaches.


### TO NOTE: Anything preceded by at # is a 'comment'
#   that means it is something ignored by R
#   which means comments are usually something like this --
#   informative statements used for later reference, such as
#   reminding yourself what you did and why!


##Going further: between group (discriminant) analyses.


library(TInPosition) ##we need the two-table versions of ExPosition and InPosition: TExPosition and TInPosition

rm(list=ls()) #clean the workspace

load('clean.ps.data.rda')
load('clean.ps.design.rda')


##  The between-groups version of PCA: Barycentric Discriminant Analysis (BADA)
### This is the data we analyze:
bada.group.averages <- apply(ps.sim.design, 1, "/", colSums(ps.sim.design)) %*% as.matrix(ps.sim.data)
	##just for reference... tepBADA does this for us!


## This is fixed effects, and therefore boring!
#bada.ps.res <- tepBADA(ps.sim.data,DESIGN=ps.sim.design,make_design_nominal=FALSE)
##Let's cut to chase.
bada.ps.res <- tepBADA.inference.battery(ps.sim.data,DESIGN=ps.sim.design,make_design_nominal=FALSE)

## The output is just like that with InPosition and ExPosition with a few critical additions:
bada.ps.res$Fixed.Data$TExPosition.Data$assign	#but, this is available via the $Inference.Data, too:

## clasification accuracy
bada.ps.res$Inference.Data$loo.data
bada.ps.res$Inference.Data$loo.data$fixed.confuse
bada.ps.res$Inference.Data$loo.data$loo.confuse

## omnibus test
bada.ps.res$Inference.Data$omni$p.val

## r2 test:
bada.ps.res$Inference.Data$r2$r2
bada.ps.res$Inference.Data$r2$p.val




##  The between-groups version of MCA: Discriminant Correspondence Analysis (DICA)
### This is the data we analyze:
dica.group.averages <- t(ps.sim.design) %*% makeNominalData(ps.sim.data)
	##just for reference... tepDICA does this for us!
	
dica.ps.res <- tepDICA.inference.battery(ps.sim.data,make_data_nominal=TRUE,DESIGN=ps.sim.design,make_design_nominal=FALSE)
dica.ps.res$Fixed.Data$TExPosition.Data$assign	#but, this is available via the $Inference.Data, too:

## clasification accuracy
dica.ps.res$Inference.Data$loo.data
dica.ps.res$Inference.Data$loo.data$fixed.confuse
dica.ps.res$Inference.Data$loo.data$loo.confuse

## omnibus test
dica.ps.res$Inference.Data$omni$p.val

## r2 test:
dica.ps.res$Inference.Data$r2$r2
dica.ps.res$Inference.Data$r2$p.val




##quick comparison:
bada.ps.res$Inference.Data$loo.data$loo.confuse
dica.ps.res$Inference.Data$loo.data$loo.confuse ##the winner!