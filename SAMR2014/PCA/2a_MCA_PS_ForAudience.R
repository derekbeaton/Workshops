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


#How would MCA treat the data?
nom.data <- makeNominalData(ps.sim.data)
nom.data[c(1:2,101:102,201:202),1:10] ## let's look at this type of coding
	## compared to
ps.sim.data[c(1:2,101:102,201:202),1:2]	

##plain ol' MCA
mca.ps.sim.fix <- epMCA(ps.sim.data,make_data_nominal=TRUE,DESIGN=ps.sim.design,make_design_nominal=FALSE)


##let's explore output & hierarcy of the output
###This is the plotting data. Just a quick reference of where plotting parameters can be changed.
mca.ps.sim.fix
mca.ps.sim.fix$Plotting.Data
mca.ps.sim.fix$Plotting.Data$fi.col

###The good stuff -- all the MCA results.
mca.ps.sim.fix$ExPosition.Data

##And important specific things like:
###explained variance
mca.ps.sim.fix$ExPosition.Data$t
	##NOT necessarily eigenvalues -- to be discussed in detail at the workshop!
	###and we can visualize this relationship
prettyScree(mca.ps.sim.fix$ExPosition.Data$eigs)	


###factor scores (and normalized loadings)
mca.ps.sim.fix$ExPosition.Data$fi
mca.ps.sim.fix$ExPosition.Data$fj

###contributions to the variance
mca.ps.sim.fix$ExPosition.Data$ci
mca.ps.sim.fix$ExPosition.Data$cj

##interpretting at other components
epGraphs(mca.ps.sim.fix,x_axis=2,y_axis=3)
		### Uh-oh. We have an interpretation here that differs in some ways from the PCA version.
		### Component 1 is qualitatively the same: paranoia vs. not
		### Component 2, though, is a bit different.
		### MCA also provides a level of detail otherwise missed in PCA.
		### We'll discuss this in full during the workshop. 



#### However, MCA in and of itself is just a descriptive model. There is no inference. 
#### Which means we don't know if the results are significant or meaningful yet.
###
#### And if we don't know what is significant... 
#### we may be interpretting null effects (as blasphemous sin in the eyes of Sir Ronald Fisher)!