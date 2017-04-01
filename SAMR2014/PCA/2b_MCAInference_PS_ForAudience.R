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



##Which directory are we in?
getwd()
	##Do we want to change our directory? Usually, this is a good idea
	# ?setwd() will help us!

## do this if you need to:
## setwd() ## -- be sure to setwd properly!

##let's see if we have anything in the workspace:
ls()


## If it's OK, let's clean the workspace.
rm(list=ls()) #clean the workspace



load('ps.sim.data.rda')
load('ps.sim.design.rda')

##let's see what is in our workspace, again!:
ls()


##Let's load up the packages we need:
#library(prettyGraphs)
#library(ExPosition)
#library(InPosition)

##But, if packages have dependences, they will load up together when we load the one with all the dependencies:
library(InPosition) ##InPosition depends on ExPosition which depends on prettyGraphs -- much easier!


#### MCA in and of itself is just a descriptive model. There is no inference. 
#### Which means we don't know if the results are significant or meaningful yet.
###
#### And if we don't know what is significant... 
#### we may be interpretting null effects (as blasphemous sin in the eyes of Sir Ronald Fisher)!


##MCA with an inference battery -- all the permutation and bootstrap tests are performed at once.
mca.ps.sim.inf <- epMCA.inference.battery(ps.sim.data,DESIGN=ps.sim.design,make_design_nominal=FALSE,test.iters=100)
##It all looks the same -- just colored differently.
	##and with a few extra things written on the pictures.
	## to be discussed in detail at the workshop

##explore output & hierarchy
mca.ps.sim.inf
mca.ps.sim.inf$Fixed.Data # -- same as above!
						  ###whatever we understood before is still here!
						  ###see, I'll prove it!
mca.ps.sim.inf$Fixed.Data$ExPosition.Data$t # -- same thing as up above!						  


##But now we have something new
mca.ps.sim.inf$Inference.Data

###First, permutation. It tells us which components are significant.
mca.ps.sim.inf$Inference.Data$components
mca.ps.sim.inf$Inference.Data$components$p.vals

###some tricks to get what we need for prettyScree
sig.comps <- (which(duplicated(cumsum(mca.ps.sim.inf$Inference.Data$components$p.vals < 0.05)))[1])-1
tot.var <- sum(mca.ps.sim.inf$Fixed.Data$ExPosition.Data$t[1:sig.comps]/100)

prettyScree(mca.ps.sim.inf$Fixed.Data$ExPosition.Data$eigs)	##so this was a bad guess!

prettyScree(mca.ps.sim.inf$Fixed.Data$ExPosition.Data$eigs,
	n.comps= sig.comps,
	broken.stick=FALSE,
	kaiser=FALSE,
	perc.exp= tot.var)	##but let's make it prettier and more accurate!
	
prettyScree(mca.ps.sim.inf$Fixed.Data$ExPosition.Data$eigs,
	n.comps= sig.comps,
	broken.stick=TRUE,
	kaiser=TRUE,
	perc.exp=tot.var)	##now with all the tests again.


### Now, the bootstrap 
### For fun: (see here for a great video interview by Efron on the bootstrap: http://www.youtube.com/watch?v=6l9V1sINzhE)

mca.ps.sim.inf$Inference.Data$fj.boots

##Bootstrap ratios and tests for Component 1
mca.ps.sim.inf$Inference.Data$fj.boots$tests$sig.boot.ratios[,1]
mca.ps.sim.inf$Inference.Data$fj.boots$tests$boot.ratios[,1]

##Bootstrap ratios and tests for Component 2
mca.ps.sim.inf$Inference.Data$fj.boots$tests$sig.boot.ratios[,2]
mca.ps.sim.inf$Inference.Data$fj.boots$tests$boot.ratios[,2]


###And now other components!
inGraphs(mca.ps.sim.inf,x_axis=2,y_axis=3)