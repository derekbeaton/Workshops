library(ggplot2)
library(InPosition)
library(ours)
library(tidyverse)
library(factoextra)

# data
load(file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","amerge_subset.rda"))
load(file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","variable_type_map.rda"))

add.alpha <- function(col, alpha=0.65){
  apply(
    sapply(col, col2rgb)/255, 
    2, 
    function(x){
      rgb(x[1], x[2], x[3], alpha=alpha)
    }
  )  
}

continuous_data <- amerge_subset[,which(variable_type_map[,"Continuous"]==1)]
ordinal_data <- amerge_subset[,which(variable_type_map[,"Ordinal"]==1 & variable_type_map[,"Categorical"]!=1)]
categorical_data <- amerge_subset[,which(variable_type_map[,"Categorical"]==1)]
  ## let's drop DX from the analysis...
  categorical_data <- categorical_data[,-c(which("DX" %in% colnames(categorical_data)))]


continuous_escofier_data <- escofier.coding(continuous_data, scale=T)
ordinal_thermometer_data <- thermometer.coding(ordinal_data)
categorical_disjunctive_data <- make.data.nominal(categorical_data)


mixed_data <- cbind(continuous_escofier_data,
                    ordinal_thermometer_data,
                    categorical_disjunctive_data)

ca_results <- epCA(mixed_data, DESIGN = amerge_subset$DX, make_design_nominal = T, graphs=F)

iterations <- 100
bootstrap_column_scores_cube <- array(NA, dim=c(ncol(mixed_data), length(ca_results$ExPosition.Data$eigs), iterations))
rownames(bootstrap_column_scores_cube) <- colnames(mixed_data)
for(i in 1:iterations){
  
  boot_sample <- sample(1:nrow(mixed_data), replace = T)
  
  bootstrap_column_scores_cube[,,i] <- caSupplementalElementsPreProcessing(t(mixed_data[boot_sample,])) %*% ca_results$ExPosition.Data$fi[boot_sample,] %*% diag(1/ca_results$ExPosition.Data$pdq$Dv)

}

bootstrap_ratios <- boot.ratio.test(bootstrap_column_scores_cube)

## show bootstrap distributions for two variables (one sig, one not) for only C1
## on a simple Component plot.
  ## hippo, age, apoe42

xlim <- c(-max(abs(bootstrap_column_scores_cube[,1,])),max(abs(bootstrap_column_scores_cube[,1,])))*1.1
ylim <- c(-max(abs(bootstrap_column_scores_cube[,2,])),max(abs(bootstrap_column_scores_cube[,2,])))*1.1
xlab <- paste0("Component 1. Explained variance: ", round(ca_results$ExPosition.Data$t[1], digits = 2),"%")
ylab <- paste0("Component 2. Explained variance: ", round(ca_results$ExPosition.Data$t[2], digits = 2),"%")

plot(0, type="n", xlim=xlim, ylim=ylim, axes=F, asp=1, xlab=xlab, ylab=ylab)
abline(h=0,lty=2,lwd=2, col="grey60")
abline(v=0,lty=2,lwd=2, col="grey60")
points(ca_results$ExPosition.Data$fj[,1:2], pch=20, cex=1.5)
text(ca_results$ExPosition.Data$fj[,1:2], labels = rownames(ca_results$ExPosition.Data$fj), pos = 3, cex=.8)


keep_non_minuses <- ifelse(grepl("\\-",rownames(ca_results$ExPosition.Data$fj)),"grey80","black")
plot(0, type="n", xlim=xlim, ylim=ylim, axes=F, asp=1, xlab=xlab, ylab=ylab)
abline(h=0,lty=2,lwd=2, col="grey60")
abline(v=0,lty=2,lwd=2, col="grey60")
points(ca_results$ExPosition.Data$fj[,1:2], pch=20, cex=1.5, col=keep_non_minuses)
text(ca_results$ExPosition.Data$fj[,1:2], labels = rownames(ca_results$ExPosition.Data$fj), pos = 3, cex=.8, col=keep_non_minuses)


bootstrap_focus <- c("Hippocampus+","AGE+","APOE4.2","PTGENDER.Female")
bootstrap_focus_colors <- c("mediumorchid4","olivedrab3","steelblue4","firebrick3")
names(bootstrap_focus_colors) <- bootstrap_focus

plot(0, type="n", xlim=xlim, ylim=ylim, axes=F, asp=1, xlab=xlab, ylab=ylab)
abline(h=0,lty=2,lwd=2, col="grey60")
abline(v=0,lty=2,lwd=2, col="grey60")
points(ca_results$ExPosition.Data$fj[bootstrap_focus,1:2], pch=21, cex=1.5, bg=bootstrap_focus_colors)
text(ca_results$ExPosition.Data$fj[bootstrap_focus,1:2], labels = rownames(ca_results$ExPosition.Data$fj[bootstrap_focus,]), pos = 3, cex=.8, col=bootstrap_focus_colors)


plot(0, type="n", xlim=xlim, ylim=ylim, axes=F, asp=1, xlab=xlab, ylab=ylab)
abline(h=0,lty=2,lwd=2, col="grey60")
abline(v=0,lty=2,lwd=2, col="grey60")
for(i in bootstrap_focus){
  points(t(bootstrap_column_scores_cube[i,1:2,]), bg=add.alpha(bootstrap_focus_colors[i]), pch=21)
}
points(ca_results$ExPosition.Data$fj[bootstrap_focus,1:2], pch=21, cex=1.5, bg=bootstrap_focus_colors)
text(ca_results$ExPosition.Data$fj[bootstrap_focus,1:2], labels = rownames(ca_results$ExPosition.Data$fj[bootstrap_focus,]), pos = 3, cex=.8, col=bootstrap_focus_colors)


plot(0, type="n", xlim=xlim, ylim=ylim, axes=F, asp=1, xlab=xlab, ylab=ylab)
abline(h=0,lty=2,lwd=2, col="grey60")
abline(v=0,lty=2,lwd=2, col="grey60")
for(i in bootstrap_focus){
  peeledHull(t(bootstrap_column_scores_cube[i,1:2,]), col = bootstrap_focus_colors[i], percentage = 1)
}
points(ca_results$ExPosition.Data$fj[bootstrap_focus,1:2], pch=21, cex=1.5, bg=bootstrap_focus_colors)
text(ca_results$ExPosition.Data$fj[bootstrap_focus,1:2], labels = rownames(ca_results$ExPosition.Data$fj[bootstrap_focus,]), pos = 3, cex=.8, col=bootstrap_focus_colors)


plot(0, type="n", xlim=xlim, ylim=ylim, axes=F, asp=1, xlab=xlab, ylab=ylab)
abline(h=0,lty=2,lwd=2, col="grey60")
abline(v=0,lty=2,lwd=2, col="grey60")
for(i in bootstrap_focus){
  peeledHull(t(bootstrap_column_scores_cube[i,1:2,]), col = bootstrap_focus_colors[i], percentage = 1)
}
points(ca_results$ExPosition.Data$fj[bootstrap_focus,1:2], pch=21, cex=1.5, bg=bootstrap_focus_colors)
text(ca_results$ExPosition.Data$fj[bootstrap_focus,1:2], labels = rownames(ca_results$ExPosition.Data$fj[bootstrap_focus,]), pos = 3, cex=.8, col=bootstrap_focus_colors)
legend("topleft", legend=round(bootstrap_ratios$boot.ratios[bootstrap_focus,1],digits=2), ncol=4, col=bootstrap_focus_colors, pch=20, pt.cex = 2, title="Component 1 Bootstrap Ratios", bty="n")
legend("bottomright", legend=round(bootstrap_ratios$boot.ratios[bootstrap_focus,2],digits=2), ncol=1, col=bootstrap_focus_colors, pch=20, pt.cex = 2, title="Component 2 Bootstrap Ratios", bty="n")

