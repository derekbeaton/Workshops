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



##Some great web references moving forward and beyond:
# http://ww2.coastal.edu/kingw/statistics/R-tutorials/formulae.html
# http://www.cookbook-r.com/Statistical_analysis/ANOVA/#mixed-design-anova
# http://personality-project.org/r/r.anova.html
# http://conjugateprior.org/2013/01/formulae-in-r-anova/

rm(list=ls())
##from a .csv

## be sure to getwd() to see where you are.
## then, setwd() to where all the files live on your computer.
part.repeat.wide.format <- read.csv('S(A)xB.csv')


##we need to reshape the data. Recall that R likes things in "long format". 
part.repeat.long.format <- reshape(
   part.repeat.wide.format,
   idvar=c("S"),
   varying=list( c("Similar","Dissimilar") ),
   v.names="y",
   times=c("Similar","Dissimilar"),
   timevar="B",   
   direction="long"
)

rownames(part.repeat.long.format) <- NULL ###these rownames from reshape are a bit cumbersome.


##Now we have a bit of a problem! R prefers that everything is a completely between subjects design. 
#That's why it was so easy to make the aov() call.

##But now we must recall the dreaded score model!
##We need to know how the F-ratio is computed for a specific design so we can determine the error.


	##Error here is determined by the within variable and the subjects.
	##proper form: summary(aov(y~(A*B)+Error(S/B)+(A),data=part.repeat.long.format))
	##The between factor here is implicit because there is just 1.
aov.res <- aov(y~(A*B)+Error(S/B),data=part.repeat.long.format)
aov.table <- summary(aov.res)
#model.tables(aov.res,"means")


dev.new()
interaction.plot(part.repeat.long.format$A,part.repeat.long.format$B,part.repeat.long.format$y,type="b",xlab="Age Group (Between)",ylab="Number of correct pairs",trace.label="Similarity (Repeated)",pch=c(20,22),cex=2,lwd=2,col=c("mediumorchid4","darkseagreen"))

##but that's not what we wanted... it's backwards!
##a quick note!
part.repeat.long.format$A <- factor(part.repeat.long.format$A,levels=sort(unique(part.repeat.long.format$A),decreasing=TRUE))


dev.new()
interaction.plot(part.repeat.long.format$A,part.repeat.long.format$B,part.repeat.long.format$y,type="b",xlab="Age Group (Between)",ylab="Number of correct pairs",trace.label="Similarity (Repeated)",pch=c(20,22),cex=2,lwd=2,col=c("mediumorchid4","darkseagreen"))

##That's better!