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



rm(list=ls())

library(ez)
library(car)


## be sure to getwd() to see where you are.
## then, setwd() to where all the files live on your computer.
load('unbalanced.sim.ps.rda')

## we are now going to deal with some between factors, some within factors, and an unbalanced design!
## triple yuck!

##reformat from "wide" to "long"
unbalanced.part.repeat.long.format <- reshape(
   unbalanced.sim.ps,
   idvar=c("S"),
   varying=list( c("TEST","RETEST") ),
   v.names="score",
   times=c("TEST","RETEST"),
   timevar="TEST.CONDITION",   
   direction="long"
)
rownames(unbalanced.part.repeat.long.format) <- NULL



##Just groups and test condition -- a partially repeated.
ez.res.part.repeat <- ezANOVA(
				data= unbalanced.part.repeat.long.format,
				wid=S,
				dv = score,
				between=.(GROUP),
				within=.(TEST.CONDITION),
				type=3,						##type 3 SS are critically important.
				return_aov=TRUE
				)

ez.res.part.repeat$ANOVA



###now all 3 factors
##if we were to even try this with aov:
	## anova(lmer(score~(1|S) + TEST.CONDITION * GROUP * GENDER,data=unbalanced.part.repeat.long.format))
	## anova(lmer(score~(1|S) + TEST.CONDITION * GENDER * GROUP,data=unbalanced.part.repeat.long.format))
	## yuck -- and, again, this is all type 1!! that means order matters.
		## if we want type 3 and mixed models (and even mixed effects), we need much more sophisticated tools or...
## a clearly easier approach:
ez.res.2.btw.1.with <- ezANOVA(
				data= unbalanced.part.repeat.long.format,
				wid=S,
				dv = score,
				between=.(GROUP,GENDER),
				within=.(TEST.CONDITION),
				type=3,						##type 3 SS are critically important for unbalanced and order independent.
				return_aov=TRUE
				)

ez.res.2.btw.1.with$ANOVA
