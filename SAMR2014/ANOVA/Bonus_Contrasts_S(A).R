##A
rm(list=ls())

one.f.wide.format <- read.csv('S(A).csv')


##B
one.f.long.format <- stack(one.f.wide.format) ##there is an alternative approach with reshape. 

colnames(one.f.long.format) <- c('y','A')
one.f.long.format$S <- paste("Subj",1:nrow(one.f.long.format),sep=".")



##C

	###what does y, ~, and A mean?
aov.res <- aov(y~A,data= one.f.long.format)
	##summary is a ubiqitous function.
aov.table <- summary(aov.res)


##What about contrasts?
scores <- one.f.long.format$y

contrast.1 <- c(rep(-3,5),rep(5,5),rep(-1,5),rep(-1,5))
lin.model.1 <- lm(scores ~ contrast.1)
summary(lin.model.1)

contrast.2 <- c(rep(-1,5),rep(0,5),rep(1,5),rep(0,5))
lin.model.2 <- lm(scores ~ contrast.2)
summary(lin.model.2)

