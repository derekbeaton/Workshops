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

library(foreign) ##this package allows us to get data from other formats


## be sure to getwd() to see where you are.
## then, setwd() to where all the files live on your computer.
two.btw.from.spss <- read.spss('S(AxB).sav')


names(two.btw.from.spss) ##let's take a look at the names of the data columns.

##now let's make the data table again.
two.f.btw.long.format <- cbind.data.frame(
			two.btw.from.spss$Subj_ID,
			two.btw.from.spss$Word_count,
			two.btw.from.spss$Recall_type,
			two.btw.from.spss$score)

colnames(two.f.btw.long.format) <- c("S","A","B","y") ##with proper names.


###just test the main effects of the two factors
# lm.res.maineffects <- lm(y~A+B,data= two.f.btw.long.format)
# aov.res.maineffects <- aov(lm.res.maineffects)
# aov.table.maineffects <- summary(aov.res.maineffects)

###just test the interaction between the two factors
# lm.res.interact <- lm(y~A:B,data= two.f.btw.long.format)
# aov.res.interact <- aov(lm.res.interact)
# aov.table.interact <- summary(aov.res.interact)

###The good stuff: main effects and interactions.
aov.res <- aov(y~A*B,data=two.f.btw.long.format)
aov.table <- summary(aov.res)


##Simple but, kind of boring!
#interaction.plot(two.f.btw.long.format$A,two.f.btw.long.format$B,two.f.btw.long.format$y)

##great for grayscale publication pictures
dev.new()
interaction.plot(two.f.btw.long.format$A,two.f.btw.long.format$B,two.f.btw.long.format$y,type="b",xlab="Word list length",ylab="Words recalled",trace.label="Cue Condition",pch=c(20,22),cex=2,lwd=2)

##but now with colors!
dev.new()
interaction.plot(two.f.btw.long.format$A,two.f.btw.long.format$B,two.f.btw.long.format$y,type="b",xlab="Word list length",ylab="Words recalled",trace.label="Cue Condition",pch=c(20,22),cex=2,lwd=2,col=c("firebrick4","steelblue4"))

##our perferred colors!
dev.new()
interaction.plot(two.f.btw.long.format$A,two.f.btw.long.format$B,two.f.btw.long.format$y,type="b",xlab="Word list length",ylab="Words recalled",trace.label="Cue Condition",pch=c(20,22),cex=2,lwd=2,col=c("mediumorchid4","darkseagreen"))
##no error bars without using ggplot2, gplot, HMisc, sciplot or other packages

##let's go straight to a PNG with this:
png("TwoFactor_InteractionPlot_pretty.png")
interaction.plot(two.f.btw.long.format$A,two.f.btw.long.format$B,two.f.btw.long.format$y,type="b",xlab="Word list length",ylab="Words recalled",trace.label="Cue Condition",pch=c(20,22),cex=2,lwd=2,col=c("mediumorchid4","darkseagreen"))
dev.off()


two.f.tukey <- TukeyHSD(aov.res,ordered=TRUE)
plot(two.f.tukey)

##pairwise t is a bit harder!
all.pair.comps <- pairwise.t.test(two.f.btw.long.format$y,paste(two.f.btw.long.format$A,two.f.btw.long.format$B,sep=":"), p.adjust.method="bonf")


##save it all
save.image("TwoFactor_SaveEverythingInWorkspace.RData")