# packages
# library()
library(ggplot2)
library(ExPosition)

# data
load(file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","amerge_subset.rda"))
load(file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","variable_type_map.rda"))

## almost all plot code entirely stolen from here so I could do this in ggplot:
# https://stats.stackexchange.com/questions/153564/visualizing-pca-in-r-data-points-eigenvectors-projections-confidence-ellipse

perp.segment.coord <- function(x0, y0, a=0,b=1){
  #finds endpoint for a perpendicular segment from the point (x0,y0) to the line
  # defined by lm.mod as y=a+b*x
  x1 <- (x0+b*y0-a*b)/(1+b^2)
  y1 <- a + b*x1
  list(x0=x0, y0=y0, x1=x1, y1=y1)
}

trails.fdg.df <- data.frame(TRAILS=amerge_subset$mPACCtrailsB, FDG=amerge_subset$FDG)
## center/normalize
trails.fdg.df_normed <- data.frame(TRAILS=expo.scale(as.matrix(amerge_subset$mPACCtrailsB),scale="SS1"), FDG=expo.scale(as.matrix(amerge_subset$FDG),scale="SS1"))

eigen_results <- eigen(crossprod(as.matrix(trails.fdg.df_normed)))
eigen_results$slopes[1] <- eigen_results$vectors[1,1]/eigen_results$vectors[2,1]  # calc slopes as ratios
eigen_results$slopes[2] <- eigen_results$vectors[1,1]/eigen_results$vectors[1,2]  # calc slopes as ratios



g <- ggplot(trails.fdg.df, mapping=aes(x = TRAILS, y = FDG)) + 
  geom_point(alpha = 1/3) + 
  coord_fixed() + 
  theme_bw()
g 


g <- ggplot(trails.fdg.df_normed, mapping=aes(x = TRAILS, y = FDG)) + 
  geom_point(alpha = 1/3) + 
  stat_ellipse(type = "norm", alpha=0) + ## such a lazy hack...
  geom_segment(data=as.data.frame(perp.segment.coord(trails.fdg.df_normed$TRAILS, trails.fdg.df_normed$FDG, 0, eigen_results$slopes[1])), aes(x = x0, y = y0, xend = x1, yend = y1), alpha = 0) +
  theme_bw() +
  coord_fixed() 
g

g <- g + geom_abline(intercept = 0, slope = eigen_results$slopes[1], colour = "mediumorchid4", size=2)  # plot pc1
g

ss1 <- perp.segment.coord(trails.fdg.df_normed$TRAILS, trails.fdg.df_normed$FDG, 0, eigen_results$slopes[1])
g <- g + geom_segment(data=as.data.frame(ss1), aes(x = x0, y = y0, xend = x1, yend = y1), colour = "mediumorchid4", size=1, alpha = 1/3)
g

## then the geom_segments()s for the lm()s
## bonus in case you want to see the regression lines.
trails.from.fdg <- lm(trails.fdg.df_normed$TRAILS ~ trails.fdg.df_normed$FDG)
fdg.from.trails <- lm(trails.fdg.df_normed$FDG ~ trails.fdg.df_normed$TRAILS)
g + 
  geom_segment(x = min(trails.fdg.df_normed$TRAILS), y = min(fdg.from.trails$fitted.values), xend = max(trails.fdg.df_normed$TRAILS), yend = max(fdg.from.trails$fitted.values), colour = "orange", size=2) +
  geom_segment(y = min(trails.fdg.df_normed$FDG), x = min(trails.from.fdg$fitted.values), yend = max(trails.fdg.df_normed$FDG), xend = max(trails.from.fdg$fitted.values), colour = "orange", size=2)
# g
# g <- g+ stat_ellipse(type = "norm")
# g

g <- g + geom_abline(intercept = 0, slope = eigen_results$slopes[2], colour = "olivedrab4", size=1)  # plot pc2
g

  ## bonus if you'd like to see how the second component fits.
# ss2 <- perp.segment.coord(trails.fdg.df_normed$TRAILS, trails.fdg.df_normed$FDG, 0, eigen_results$slopes[2])
# g <- g + geom_segment(data=as.data.frame(ss2), aes(x = x0, y = y0, xend = x1, yend = y1), colour = "olivedrab4", size=.75)
# g

