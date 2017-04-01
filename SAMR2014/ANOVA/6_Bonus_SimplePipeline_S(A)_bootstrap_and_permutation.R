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

##some resources: http://www.usna.edu/Users/math/jager/courses/sm439/labs/Lab7.pdf
##also, there are packages for this: e.g., lmp


rm(list=ls())
##from a .csv
		###NOTE: This is usually how we would "think" of the data: partitioned by groups.


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
lm.res <- lm(y~A,data= one.f.long.format)
aov.table <- anova(lm.res)


observed.F <- summary(lm.res)$fstatistic[1]
observed.r2 <- summary(lm.res)$r.squared

##permutation for F
perm.one.f.long.format <- one.f.long.format
permutation.iterations <- 1000

perm.F.values <- vector("numeric", permutation.iterations)
perm.R2.values <- vector("numeric", permutation.iterations)
for(i in 1: permutation.iterations){
	
	perm.one.f.long.format$A <- sample(perm.one.f.long.format$A)
	perm.lm.res <- lm(y~A,data=perm.one.f.long.format)
	perm.F.values[i] <- summary(perm.lm.res)$fstatistic[1]
	perm.R2.values[i] <- summary(perm.lm.res)$r.squared	
	
}

p.val <- max(sum(perm.F.values > observed.F)/1000,1/permutation.iterations) ##compare that to the parametric one!
cutoff <- round(.95* permutation.iterations)

dev.new()
hist(perm.F.values,breaks=100,col="purple",border="white",xlab="F Values")
abline(v=sort(perm.F.values)[cutoff],col="red",lwd=3,lty=2)
abline(v=observed.F,col="springgreen2",lwd=4)

dev.new()
hist(perm.R2.values,breaks=50,col="purple",border="white",xlab="R^2",xlim=c(-0.1,1.1))
abline(v=sort(perm.R2.values)[cutoff],col="red",lwd=3,lty=2)
abline(v= observed.r2,col="springgreen2",lwd=4)


##bootstrap of R^2 and F
boot.iterations <- 1000
boot.F.values <- vector("numeric", boot.iterations)
boot.R2.values <- vector("numeric", boot.iterations)
for(i in 1: boot.iterations){
	
	one.f.wide.boot <- as.data.frame(apply(one.f.wide.format,2,sample,replace=TRUE))
	one.f.long.boot <- stack(one.f.wide.boot) ##there is an alternative approach with reshape. 
	colnames(one.f.long.boot) <- c('y','A')
	
	#boot.one.f.long.format$A <- sample(boot.one.f.long.format$A)
	boot.lm.res <- lm(y~A,data=one.f.long.boot)
	boot.F.values[i] <- summary(boot.lm.res)$fstatistic[1]
	boot.R2.values[i] <- summary(boot.lm.res)$r.squared	
	
}

upper.tail <- .975 * boot.iterations
lower.tail <- .025 * boot.iterations

dev.new()
hist(boot.F.values,breaks=100,col="purple",border="white",xlab="F Values")
abline(v=sort(boot.F.values)[upper.tail],col="red",lwd=3,lty=2)
abline(v=sort(boot.F.values)[lower.tail],col="red",lwd=3,lty=2)
abline(v= observed.F,col="springgreen2",lwd=4)

dev.new()
hist(boot.R2.values,breaks=50,col="purple",border="white",xlab="R^2",xlim=c(-0.1,1.1))
abline(v=sort(boot.R2.values)[upper.tail],col="red",lwd=3,lty=2)
abline(v=sort(boot.R2.values)[lower.tail],col="red",lwd=3,lty=2)
abline(v= observed.r2,col="springgreen2",lwd=4)