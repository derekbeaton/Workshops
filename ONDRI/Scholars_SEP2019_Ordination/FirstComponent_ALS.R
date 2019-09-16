
### maybe th.new should be random?
# th.new = X.old[, 1]
### a random guess! show first our "random guess" of where we think factor scores would be
### which is where the dots correspond to on the line
## so this should be a straight shot

## show the projection & links to the line
### show the loadings/coefficients (plus the multiplication)
### show the computation of the eigvals

### start the story with "loadings" and "component scores"
### show them in their boxes by way of SVD
### later tell people about the eigen

### go to bigger example
### then we conclude with "what does the component mean?"
### then scale up to something with a few more variables
### plot it and ask "what does it mean?"

library(ExPosition)

set.seed(101)

perp.segment.coord <- function(x0, y0, a=0, b=1){
  x1 <- (x0+b*y0-a*b)/(1+b^2)
  y1 <- a + b*x1
  list(x0=x0, y0=y0, x1=x1, y1=y1)
}

data("words",package="ExPosition")
load("/Data/ADNI/Examples/amerge_subset.rda")
# trails.fdg.df <- data.frame(TRAILS=amerge_subset$mPACCtrailsB, FDG=amerge_subset$FDG)
# trails.fdg.df_normed <- scale(as.matrix(data.frame(TRAILS=amerge_subset$mPACCtrailsB, FDG=amerge_subset$FDG)))

X <- scale(words$data)
colnames(X) <- c("Hippocampus Volume","Trails B (time)")
rownames(X) <- paste0("SUBJ_", 1:nrow(X))
# X <- trails.fdg.df_normed

# pheatmap(X, cluster_rows = F, cluster_cols = F, legend = F, col=viridisLite::cividis(100) )

n = nrow(X)
p = ncol(X)
nc <- 1
X.old = X
Th = matrix(NA, n, nc)
Ph = matrix(NA, p, nc)
eigvals = rep(NA, nc)

# h <- 1
  
th.stores <- th.new <- rnorm(nrow(X.old))
# th.orig <- th.new <- runif(nrow(X.old))
ph.old = rep(1, p)
ph.stores <- ph.new <- c(-.8, -.4)#rep(1, p)


resids <- c(sum( (X.old - th.new %*% t(ph.new))^2))

  ### I need to store all the ph/news so I can re-draw them every time...

iter = 1
repeat {
    ### this is X'y
  ph.new = t(X.old) %*% th.new/sum(th.new^2)
  ph.new = ph.new/sqrt(sum(ph.new^2))
    ### this is Xb
  th.new = X.old %*% ph.new
  resids <- c(resids, sum( (X.old - th.new %*% t(ph.new))^2) )
  
  ph.stores <- cbind(ph.stores, ph.new)
  th.stores <- cbind(th.stores, th.new)
  
  ph.aux = sum((ph.new - ph.old)^2)
  ph.old = ph.new
  if (ph.aux < 1e-12 || iter == 1000) 
    break
  iter = iter + 1
  
}




# plot(scale(words$data), axes=F, pch=20, asp=NA)
# abline(h=0,col="black")
# abline(v=0,col="black")
# abline(a=0, b=ph.new[2] / ph.new[1], col="green", lty=2)
# ss <- perp.segment.coord(X.old[,1], X.old[,2], a = 0, b = ph.new[2] / ph.new[1])
# segments(ss$x0, ss$y0, ss$x1, ss$y1, col="darkorchid")


## so I have to have a base plot
  ## then move through iters
  ## but when iters > 1, I need to also make a new plot with all previous iters greyed out





## base color: black
## watch them move: green
## see them stop: red
## their pathway: darkorchid
## grey out in between

## need a "dot distance" and a "dot spread"
  ## sum of squared distance is in darkorchid
  ## sum of squared spread is red


contributions <- round(((ph.stores^2) * sign(ph.stores)) * 100, digits = 4)

png_counter <- 1

# png(paste0("./images/ALS/",png_counter,".png"), width = 700, height = 700, pointsize = 20)
plot(X, axes=F, pch=20, col="white", asp=1)
abline(h=0,col="black", lty=1, lwd=1)
abline(v=0,col="black", lty=1, lwd=1)
points(X, pch=21, bg="goldenrod", col="steelblue4", cex = 1)
#dev.off()


png_counter <- png_counter + 1
# png(paste0("./images/ALS/",png_counter,".png"), width = 700, height = 700, pointsize = 20)
plot(X, axes=F, pch=20, col="white", asp=1, main="A (terrible) component?")
abline(h=0,col="black", lty=1, lwd=1)
abline(v=0,col="black", lty=1, lwd=1)
points(X, pch=21, bg="goldenrod", col="steelblue4", cex = 1)
abline(a=0, b=ph.stores[2,1] / ph.stores[1,1], col="olivedrab3", lty=1, lwd = 2)
#dev.off()

## these contributions are loadings
### which are cosines, or angles away from where they started; and exist between -1 and 1

png_counter <- png_counter + 1
# png(paste0("./images/ALS/",png_counter,".png"), width = 700, height = 700, pointsize = 20)
plot(X, axes=F, pch=20, col="white", asp=1, main="Loadings (are angles)", xlab=paste0("Hippocampus Volume: Loading = ", round(ph.stores[1,1], digits=3)), ylab=paste0("Trails B (time): Loading = ", round(ph.stores[2,1], digits=3)))
abline(h=0,col="black", lty=1, lwd=1)
abline(v=0,col="black", lty=1, lwd=1)
points(X, pch=21, bg="goldenrod", col="steelblue4", cex = 1)
abline(a=0, b=ph.stores[2,1] / ph.stores[1,1], col="olivedrab3", lty=1, lwd = 2)
# legend("bottomleft", legend = paste0("Spread: ", round(sum(th.stores[,1]^2),digits=3)), text.col="firebrick3", bty="o", bg="white")
# legend("topright", legend = paste0("Distance: ", round(resids[1],digits=3)), text.col="darkorchid", bty="o", bg="white")
#dev.off()

### make these into a gif?

for(i in 1:ncol(ph.stores)){

  png_counter <- png_counter + 1
  # png(paste0("./images/ALS/",png_counter,".png"), width = 700, height = 700, pointsize = 20)
  plot(X, axes=F, pch=20, col="white", asp=1)
  abline(h=0,col="black", lty=1, lwd=1)
  abline(v=0,col="black", lty=1, lwd=1)
  points(X, pch=21, bg="goldenrod", col="steelblue4", cex = 1)  
  abline(a=0, b=ph.stores[2,i] / ph.stores[1,i], col="olivedrab3", lty=1, lwd = 2)
  
  if(i != 1){
    
    for(j in 1:(i-1)){
      abline(a=0, b=ph.stores[2,j] / ph.stores[1,j], col="grey40", lty=2)
      jss <- perp.segment.coord(X.old[,1], X.old[,2], a = 0, b = ph.stores[2,j] / ph.stores[1,j])
      # segments(ss$x0, ss$y0, ss$x1, ss$y1, col="grey40")
      # points(jss$x1, jss$y1, col="grey40", pch=20)
    }
    
  }
  legend("bottomleft", legend = paste0("Spread: ", round(sum(th.stores[,i]^2),digits=3)), text.col="firebrick3", bty="o", bg="white")
  legend("topright", legend = paste0("Distance: ", round(resids[i],digits=3)), text.col="darkorchid", bty="o", bg="white")
  #dev.off()
  
  png_counter <- png_counter + 1
  # png(paste0("./images/ALS/",png_counter,".png"), width = 700, height = 700, pointsize = 20)
  plot(X, axes=F, pch=20, col="white", asp=1)
  abline(h=0,col="black", lty=1, lwd=1)
  abline(v=0,col="black", lty=1, lwd=1)
  points(X, pch=21, bg="goldenrod", col="steelblue4", cex = 1)
  
  
  if(i != 1){
    
    for(j in 1:(i-1)){
      abline(a=0, b=ph.stores[2,j] / ph.stores[1,j], col="grey40", lty=2)
      jss <- perp.segment.coord(X.old[,1], X.old[,2], a = 0, b = ph.stores[2,j] / ph.stores[1,j])
      # segments(ss$x0, ss$y0, ss$x1, ss$y1, col="grey40")
      # points(jss$x1, jss$y1, col="grey40", pch=20)
    }
    
  }
  
  legend("bottomleft", legend = paste0("Spread: ", round(sum(th.stores[,i]^2),digits=3)), text.col="firebrick3", bty="o", bg="white")
  legend("topright", legend = paste0("Distance: ", round(resids[i],digits=3)), text.col="darkorchid", bty="o", bg="white")
  abline(a=0, b=ph.stores[2,i] / ph.stores[1,i], col="olivedrab3", lty=1, lwd = 2)
  ss <- perp.segment.coord(X.old[,1], X.old[,2], a = 0, b = ph.stores[2,i] / ph.stores[1,i])
  segments(ss$x0, ss$y0, ss$x1, ss$y1, col="darkorchid")
  points(ss$x1, ss$y1, col="firebrick3", pch=20)
  legend("bottomleft", legend = paste0("Spread: ", round(sum(th.stores[,i]^2),digits=3)), text.col="firebrick3", bty="o", bg="white")
  legend("topright", legend = paste0("Distance: ", round(resids[i],digits=3)), text.col="darkorchid", bty="o", bg="white")
  #dev.off()
}


## final image here with the green line, red dots, darkorchid segs, and then orange boxes underneath
png_counter <- png_counter + 1
# png(paste0("./images/ALS/",png_counter,".png"), width = 700, height = 700, pointsize = 20)
plot(X, axes=F, pch=20, col="white", asp=1, main="Et Voila!")
abline(h=0,col="black", lty=1, lwd=1)
abline(v=0,col="black", lty=1, lwd=1)
points(X, pch=21, bg="goldenrod", col="steelblue4", cex = 1)  
abline(a=0, b=ph.stores[2,i] / ph.stores[1,i], col="olivedrab3", lty=1, lwd = 2)
ss <- perp.segment.coord(X.old[,1], X.old[,2], a = 0, b = ph.stores[2,i] / ph.stores[1,i])
segments(ss$x0, ss$y0, ss$x1, ss$y1, col="darkorchid")
points(ss$x1, ss$y1, col="firebrick3", pch=20)
legend("bottomleft", legend = paste0("Spread: ", round(sum(th.stores[,i]^2),digits=3)), text.col="firebrick3", bty="o", bg="white")
legend("topright", legend = paste0("Distance: ", round(resids[i],digits=3)), text.col="darkorchid", bty="o", bg="white")
#dev.off()


png_counter <- png_counter + 1
# png(paste0("./images/ALS/",png_counter,".png"), width = 700, height = 700, pointsize = 20)
plot(X, axes=F, pch=20, col="white", asp=1, main="Best fit of rectangles")
abline(h=0,col="black", lty=1, lwd=1)
abline(v=0,col="black", lty=1, lwd=1)
points(X, pch=21, bg="goldenrod", col="steelblue4", cex = 1)  
abline(a=0, b=ph.stores[2,i] / ph.stores[1,i], col="olivedrab3", lty=1, lwd = 2)
segments(X.old[,1], X.old[,2], X.old[,1], rep(0,nrow(X.old)), col="goldenrod")
segments(X.old[,1], X.old[,2], rep(0,nrow(X.old)), X.old[,2], col="goldenrod")
# points(ss$x1, ss$y1, col="firebrick3", pch=20)
legend("bottomleft", legend = paste0("Spread: ", round(sum(th.stores[,i]^2),digits=3)), text.col="firebrick3", bty="o", bg="white")
legend("topright", legend = paste0("Distance: ", round(resids[i],digits=3)), text.col="darkorchid", bty="o", bg="white")
#dev.off()


png_counter <- png_counter + 1
# png(paste0("./images/ALS/",png_counter,".png"), width = 700, height = 700, pointsize = 20)
## these contributions are loadings
### which are cosines, or angles away from where they started; and exist between -1 and 1
### which also tell us how much % to each component and which direction...
plot(X, axes=F, pch=20, col="white", asp=1, main="Loadings & Contributions", xlab=paste0("Hippocampus Volume: Loading = ", round(ph.stores[1,ncol(ph.stores)], digits=3)), ylab=paste0("Trails B (time): Loading = ", round(ph.stores[2,ncol(ph.stores)], digits=3)))
abline(h=0,col="black", lty=1, lwd=1)
abline(v=0,col="black", lty=1, lwd=1)
points(X, pch=21, bg="goldenrod", col="steelblue4", cex = 1)  
abline(a=0, b=ph.stores[2,i] / ph.stores[1,i], col="olivedrab3", lty=1, lwd = 2)
#dev.off()


png_counter <- png_counter + 1
# png(paste0("./images/ALS/",png_counter,".png"), width = 700, height = 700, pointsize = 20)
plot(X, axes=F, pch=20, col="white", asp=1, main="Contributions\nare signed squared loadings * 100", xlab=paste0("Hippocampus Volume: Squared Loadings = ", round(contributions[1,ncol(ph.stores)], digits=3), "%"), ylab=paste0("Trails B (time): Squared Loadings = ", round(contributions[2,ncol(ph.stores)], digits=3),"%"))
abline(h=0,col="black", lty=1, lwd=1)
abline(v=0,col="black", lty=1, lwd=1)
points(X, pch=21, bg="goldenrod", col="steelblue4", cex = 1)  
abline(a=0, b=ph.stores[2,i] / ph.stores[1,i], col="olivedrab3", lty=1, lwd = 2)
#dev.off()

# eigen_results$slopes[1] <- eigen_results$vectors[1,1]/eigen_results$vectors[2,1]  # calc slopes as ratios
# eigen_results$slopes[2] <- eigen_results$vectors[1,1]/eigen_results$vectors[1,2]  # calc slopes as ratios

png_counter <- png_counter + 1
# png(paste0("./images/ALS/",png_counter,".png"), width = 700, height = 700, pointsize = 20)
plot(X, axes=F, pch=20, col="white", asp=1, main="The second component")
abline(h=0,col="black", lty=1, lwd=1)
abline(v=0,col="black", lty=1, lwd=1)
points(X, pch=21, bg="goldenrod", col="steelblue4", cex = 1)  
abline(a=0, b=ph.stores[2,i] / ph.stores[1,i], col="olivedrab3", lty=1, lwd = 2)
abline(a=0, b=-c(ph.stores[1,i] / ph.stores[2,i]), col="black", lty=1, lwd = 1)
abline(a=0, b=-c(ph.stores[1,i] / ph.stores[2,i]), col="olivedrab3", lty=2, lwd = 1)
#dev.off()


### and now what about the second?
  ### here it's easy
  ### but in more dimensions/variables we still rely on regression
    ### we used regressin here to *fit*.
    ### once we've fit things nicely (first component) we then take the *residuals*, or remove the first component's information from the data
    ### thus what's left is orthogonal to comp 1



### I think maybe only introduce the loadings at the very end... 
  ### it's OK to do so once in the beginning and once at the end
  ### that provides a bit of context

# system("magick -delay 80 -loop 1 ./images/ALS/for_gif/*.png component.gif")


pca_res <- epPCA(X,center = F, scale = F, graphs=F)

png(paste0("./images/ALS/PCA.png"), width = 1400, height = 700, pointsize = 20)
plot(pca_res$ExPosition.Data$fi, axes=F, pch=20, col="white", asp=NA, main="Principal components analysis",
     xlab = paste0("Component 1: ", round(pca_res$ExPosition.Data$t[1],digits=2),"% of total variance"),
     ylab = paste0("Component 2: ", round(pca_res$ExPosition.Data$t[2],digits=2),"% of total variance"))
abline(h=0, col="olivedrab3", lty=1, lwd = 2)
abline(v=0, col="black", lty=1, lwd = 1)
abline(v=0, col="olivedrab3", lty=2, lwd = 1)
points(pca_res$ExPosition.Data$fi, pch=21, bg="goldenrod", col="steelblue4", cex = 1)
dev.off()
