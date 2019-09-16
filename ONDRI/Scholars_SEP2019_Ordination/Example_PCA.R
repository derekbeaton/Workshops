### now a PCA for interpretation: stolen from PCA/MCA workshop

library(ggplot2)
library(ExPosition)
library(tidyverse)
library(factoextra)
library(ggcorrplot)
library(ggrepel)
library(pheatmap)

load(file=paste0("/Data/ADNI/Examples/amerge_subset.rda"))
load(file=paste0("/Data/ADNI/Examples/variable_type_map.rda"))

# select the continuous only subset
continuous_data_subset <- amerge_subset[,which(variable_type_map[,"Continuous"]==1)]
pca_results <- epPCA(continuous_data_subset, graphs=F)

rownames(continuous_data_subset) <- NULL

pheatmap(scale(continuous_data_subset), cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F)

## the factoextra way
scree_plot <- fviz_screeplot(pca_results, addlabels = TRUE)
scree_plot

var_cors <- fviz_pca_var(pca_results) +
  xlab(paste0("Component 1. Explained variance: ", round(pca_results$ExPosition.Data$t[1], digits=2),"%")) +
  ylab(paste0("Component 2. Explained variance: ", round(pca_results$ExPosition.Data$t[2], digits=2),"%")) +
  ggtitle("PCA:\nVariable-Component Correlations")

var_cors

# corrplot.mixed(cor(continuous_data_subset), lower="number", upper="pie", tl.pos = "lt")
ggcorrplot(cor(continuous_data_subset), hc.order = TRUE, type = "lower", lab = TRUE, outline.col = "white") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

comp_scores <- fviz_pca_ind(pca_results, alpha.ind = 1/3, label="none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  xlab(paste0("Component 1. Explained variance: ", round(pca_results$ExPosition.Data$t[1], digits=2),"%")) +
  ylab(paste0("Component 2. Explained variance: ", round(pca_results$ExPosition.Data$t[2], digits=2),"%")) +
  ggtitle("PCA:\nParticipants Component Scores")

comp_scores

comp_scores_groups <- fviz_pca_ind(pca_results, alpha.ind = 1/3, label="none", habillage = amerge_subset$DX) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  xlab(paste0("Component 1. Explained variance: ", round(pca_results$ExPosition.Data$t[1], digits=2),"%")) +
  ylab(paste0("Component 2. Explained variance: ", round(pca_results$ExPosition.Data$t[2], digits=2),"%")) +
  ggtitle("PCA:\nParticipants Component Scores")

comp_scores_groups



## an alternative way through ggplot2

participant_component_scores_long_df <- data.frame(
  COMPONENT_SCORES = c(pca_results$ExPosition.Data$fi),
  COMPONENTS = rep(as.character(1:ncol(pca_results$ExPosition.Data$fi)), each=nrow(pca_results$ExPosition.Data$fi))
)
variable_correlations_long_df <- data.frame(
  CORRELATIONS = c(cor(pca_results$ExPosition.Data$X, pca_results$ExPosition.Data$fi)),
  COMPONENTS = rep(as.character(1:ncol(pca_results$ExPosition.Data$fj)), each=nrow(pca_results$ExPosition.Data$fj)),
  NAMES = rep(as.character(rownames(pca_results$ExPosition.Data$fj)), nrow(pca_results$ExPosition.Data$fj))
)

ggplot( participant_component_scores_long_df, aes(x = 0, y = COMPONENT_SCORES) ) + 
  geom_violin(draw_quantiles = c(.25, .5, .75), adjust = .5, trim = T) + 
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1, alpha=1/5, binwidth = 1/10, color = "mediumorchid4") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  facet_grid(~factor(COMPONENTS))

ggplot( variable_correlations_long_df, aes(x = 0, y = CORRELATIONS, label=NAMES) ) + 
  geom_point() + 
  # geom_violin(draw_quantiles = c(.25, .5, .75), adjust = .5, trim = T) + 
  # geom_dotplot(binaxis='y', stackdir='center', dotsize=1, alpha=1/5, binwidth = 1/10, color = "mediumorchid4") + 
  geom_text_repel(size=3.5, box.padding = 0.5, point.padding = 0.5, direction = "both") +
  coord_cartesian(ylim = c(-1.1,1.1)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  facet_grid(~factor(COMPONENTS))