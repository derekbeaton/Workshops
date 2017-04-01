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


##what if we just did aov?

##The old (but incorrect!) way.
aov.res <- aov(TEST~GROUP*GENDER,data= unbalanced.sim.ps)
	## and what happens when we switch the order of the factors?
aov.res2 <- aov(TEST~GENDER*GROUP,data= unbalanced.sim.ps)



##The new (and more correct!) way.
ez.res <- ezANOVA(
				data=unbalanced.sim.ps,
				wid=S,
				dv = TEST,
				between=.(GROUP,GENDER),
				type=3,						##type 3 SS are critically important.
				return_aov=TRUE
				)

	## and what happens when we switch the order of the factors?				
ez.res2 <- ezANOVA(
				data=unbalanced.sim.ps,
				wid=S,
				dv = TEST,
				between=.(GENDER,GROUP),
				type=3,						##type 3 SS are critically important.
				return_aov=TRUE
				)
				


#uh-oh!!! we have a problem!
summary(aov.res)
summary(aov.res2)
ez.res$ANOVA
ez.res2$ANOVA


## ez comes with some utilities that plot for you
## these are based on ggplot2
dev.new()
ezPlot(
		data=unbalanced.sim.ps,
		wid=S,
		dv = TEST,
		x= .(GROUP),
		split =.(GENDER),
		between=.(GROUP,GENDER)
		)


##but we can still use the old way!		
dev.new()
interaction.plot(unbalanced.sim.ps$GROUP, unbalanced.sim.ps$GENDER, unbalanced.sim.ps$TEST,type="b",xlab="GROUP",ylab="PS Score",trace.label="Gender",pch=c(20,22),cex=2,lwd=2,col=c("mediumorchid4","darkseagreen"))		


##But we can still use some of the old tricks!
two.f.tukey <- TukeyHSD(ez.res$aov,ordered=TRUE)
plot(two.f.tukey)


##pairwise t is a bit harder!
all.pair.comps <- pairwise.t.test(unbalanced.sim.ps$TEST,paste(unbalanced.sim.ps$GROUP, unbalanced.sim.ps$GENDER,sep=":"), p.adjust.method="bonf")
