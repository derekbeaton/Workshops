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
##from a .csv


## be sure to getwd() to see where you are.
## then, setwd() to where all the files live on your computer.
one.f.wide.format <- read.csv('S(A).csv')


##some descriptives
colnames(one.f.wide.format)
	#or names(one.f.wide.format)
grp.means <- lapply(one.f.wide.format,mean) ##colMeans
grp.sd <- lapply(one.f.wide.format,sd)
grp.se <- lapply(grp.sd,'/',sqrt(nrow(one.f.wide.format)))
summary(one.f.wide.format)


one.f.long.format <- stack(one.f.wide.format) ##there is an alternative approach with reshape. 
colnames(one.f.long.format) <- c('y','A')
#S <- paste("Subj",1:nrow(one.f.long.format),sep=".")
#one.f.long.format <- cbind(one.f.long.format,S)
##or, more simply:
one.f.long.format$S <- paste("Subj",1:nrow(one.f.long.format),sep=".")

##Time to ANOVA!
aov.res <- aov(y~A,data= one.f.long.format)
aov.table <- summary(aov.res)
	#####alternatives:
	## 1:
	## lm.res <- lm(y~A,data=one.f.long.format)
	## aov.res <- aov(lm.res)
	## aov.table <- summary(aov.res)
	#####
	## 2:
	## lm.res <- lm(y~A,data=one.f.long.format)
	## aov.table <- anova(lm.res)
		

##save our results!
#there are two ways we recommend. First, let's save just the findings:
save(aov.res,file="aov.res.rda")
	#we'll see the second way at the end.

#plot results
boxplot(y~A,data=one.f.long.format)
library(gplots)
plotmeans(y~A,xlab="Context Group",ylab="Recall", main="Mean Plot\nwith 95% CI",data=one.f.long.format)


##post hoc tests
##Tukey
TukeyHSD.res <- TukeyHSD(aov.res,ordered=TRUE)
plot(TukeyHSD.res)
##pairwise t
all.pair.comps <- pairwise.t.test(one.f.long.format$y, one.f.long.format$A, p.adjust.method="bonf")


##second way of saving: save everything!!
	# first, let's check what we have:
	ls()
	#now, let's save it:
save.image("SimplePipeline_OneFactor_SaveEverything.RData")

