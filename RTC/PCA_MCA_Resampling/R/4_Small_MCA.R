library(ggplot2)
library(ExPosition)
library(factoextra)

# data
load(file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","amerge_subset.rda"))
load(file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","variable_type_map.rda"))

## cover other names... but then also explain why we can't just do whatever we want.
  ## effectively lead up to disj/one-hot, do a PCA, then curveball it.
  ## what do we usually do with two categorical variables?
    ## this can allow me to say "CA/MCA are Chi2 PCAs. No math, no theory here (for the viewer) but
    ## you now know: you have to be very cautious with your data.

# select specific categorical data
apoe_dx_data <- amerge_subset[,c("DX","PTRACCAT")]
apoe_dx_disjunctive_data <- makeNominalData(apoe_dx_data)

## show these data
apoe_dx_disjunctive_data

## show PCA
pca_results <- epPCA(apoe_dx_disjunctive_data, graphs=F)
# why this is wrong...
  ## it can only be "right" if we assume that the correlation matrix is meaningful
scree_plot <- fviz_screeplot(pca_results, addlabels = TRUE)
scree_plot

var_cors <- fviz_pca_var(pca_results) +
  xlab(paste0("Component 1. Explained variance: ", round(pca_results$ExPosition.Data$t[1], digits=2),"%")) +
  ylab(paste0("Component 2. Explained variance: ", round(pca_results$ExPosition.Data$t[2], digits=2),"%")) +
  ggtitle("PCA:\nVariable-Component Correlations")

var_cors

## we don't need this.
# comp_scores <- fviz_pca_ind(pca_results, alpha.ind = 1/3, label="none") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank()) +
#   xlab(paste0("Component 1. Explained variance: ", round(pca_results$ExPosition.Data$t[1], digits=2),"%")) +
#   ylab(paste0("Component 2. Explained variance: ", round(pca_results$ExPosition.Data$t[2], digits=2),"%")) +
#   ggtitle("PCA:\nParticipant Component Scores")
# 
# comp_scores

  ## is this meaningful?
  cor(apoe_dx_disjunctive_data)
  # cov(apoe_dx_disjunctive_data)
  # vs. 
  ## let's take a look at something more akin to how we'd approach this: crosstabs.
  crossprod(apoe_dx_disjunctive_data)
  ## maybe show these?
  # ours::ca.preproc(crossprod(apoe_dx_disjunctive_data))$weightedZx
  # crossprod(ours::ca.preproc(apoe_dx_disjunctive_data)$weightedZx)
  
  ## also need to show the chisq observed, expected, and deviations... 
  
  
## now explain CA
    ## and that there is something special about the row & column component scores; this is a bivariate technique.
ca_results <- epCA(apoe_dx_disjunctive_data, graphs=F)

fviz_screeplot(ca_results, addlabels = TRUE)

fviz_ca_col(ca_results) +
  coord_cartesian(xlim=c(ca_results$Plotting.Data$constraints$minx, ca_results$Plotting.Data$constraints$maxx), ylim = c(ca_results$Plotting.Data$constraints$miny, ca_results$Plotting.Data$constraints$maxy)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  xlim(c(ca_results$Plotting.Data$constraints$minx, ca_results$Plotting.Data$constraints$maxx)) +
  ylim(c(ca_results$Plotting.Data$constraints$miny, ca_results$Plotting.Data$constraints$maxy)) +
  xlab(paste0("Component 1. Explained variance: ", round(ca_results$ExPosition.Data$t[1], digits=2),"%")) +
  ylab(paste0("Component 2. Explained variance: ", round(ca_results$ExPosition.Data$t[2], digits=2),"%")) +
  ggtitle("CA:\nVariable Component Scores")

fviz_ca_row(ca_results) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  xlim(c(ca_results$Plotting.Data$constraints$minx, ca_results$Plotting.Data$constraints$maxx)) +
  ylim(c(ca_results$Plotting.Data$constraints$miny, ca_results$Plotting.Data$constraints$maxy)) +
  xlab(paste0("Component 1. Explained variance: ", round(ca_results$ExPosition.Data$t[1], digits=2),"%")) +
  ylab(paste0("Component 2. Explained variance: ", round(ca_results$ExPosition.Data$t[2], digits=2),"%")) +
  ggtitle("CA:\nParticipants Component Scores")


## there is a difference between these two.
mca_results <- epMCA(apoe_dx_data, graphs=F)
## explain that MCA is a correction for CA in the case of:
# apoe_dx_data
# # vs.
# apoe_dx_disjunctive_data


## pause to explain that you can use either
  ## and in some cases we don't know how to correct
## but that you need to be cautious on what you interpret and how (we'll come back to this later)

fviz_screeplot(mca_results, addlabels=TRUE)

fviz_mca_var(mca_results) +
  coord_cartesian(xlim=c(mca_results$Plotting.Data$constraints$minx, mca_results$Plotting.Data$constraints$maxx), ylim = c(mca_results$Plotting.Data$constraints$miny, mca_results$Plotting.Data$constraints$maxy)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  xlab(paste0("Component 1. Explained variance: ", round(mca_results$ExPosition.Data$t[1], digits=2),"%")) +
  ylab(paste0("Component 2. Explained variance: ", round(mca_results$ExPosition.Data$t[2], digits=2),"%")) +
  ggtitle("MCA:\nVariable Component Scores")

fviz_mca_ind(mca_results) +
  #coord_cartesian(xlim=c(mca_results$Plotting.Data$constraints$minx, mca_results$Plotting.Data$constraints$maxx), ylim = c(mca_results$Plotting.Data$constraints$miny, mca_results$Plotting.Data$constraints$maxy)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  xlim(c(mca_results$Plotting.Data$constraints$minx, mca_results$Plotting.Data$constraints$maxx)) +
  ylim(c(mca_results$Plotting.Data$constraints$miny, mca_results$Plotting.Data$constraints$maxy)) +
  xlab(paste0("Component 1. Explained variance: ", round(mca_results$ExPosition.Data$t[1], digits=2),"%")) +
  ylab(paste0("Component 2. Explained variance: ", round(mca_results$ExPosition.Data$t[2], digits=2),"%")) +
  ggtitle("MCA:\nParticipants Component Scores")

  ## show this...
unique(apoe_dx_disjunctive_data)


### show how there is a slight change in interpretation here.
  ## in PCA, middle meant "average", here it means "most frequent"
  ## which can be interpretted as "average, with respect to the assumptions of independence"
    ## because this is what we'd "expect"
## in PCA far meant "high variance", here it means "rare"
  ## which can be interpretted as "high variance, with respect to the assumptions of independence"


cor(mca_results$ExPosition.Data$fi, pca_results$ExPosition.Data$fi)
cor(mca_results$ExPosition.Data$fi, ca_results$ExPosition.Data$fi)
cor(ca_results$ExPosition.Data$fi, pca_results$ExPosition.Data$fi)

## so now the tough part: CA or MCA? How and when to correct?
  ## let the software do it for you. only works for strictly categorical data.
