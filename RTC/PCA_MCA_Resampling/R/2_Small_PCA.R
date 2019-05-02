library(ggplot2)
library(ExPosition)
library(tidyverse)
library(factoextra)

# data
load(file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","amerge_subset.rda"))
load(file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","variable_type_map.rda"))

perp.segment.coord <- function(x0, y0, a=0,b=1){
  # finds endpoint for a perpendicular segment from the point (x0,y0) to the line
  # defined by lm.mod as y=a+b*x
  x1 <- (x0+b*y0-a*b)/(1+b^2)
  y1 <- a + b*x1
  list(x0=x0, y0=y0, x1=x1, y1=y1)
}

amerge_subset %>%
  select(mPACCtrailsB, FDG) ->
  trails_fdg_data

pca_results <- epPCA(trails_fdg_data, graphs=F)

## first: the ggplot2 way for consistency with the previous graphs

participant_component_scores <- data.frame(COMPONENT_1=pca_results$ExPosition.Data$fi[,1], 
                                   COMPONENT_2=pca_results$ExPosition.Data$fi[,2])
g <- ggplot(participant_component_scores, mapping=aes(x = COMPONENT_1, y = COMPONENT_2)) + 
  geom_hline(yintercept = 0, color = "mediumorchid4", size=2) + 
  geom_vline(xintercept = 0, color = "olivedrab4", size=1) + 
  geom_point(alpha = 1/3) + 
  geom_segment(data=as.data.frame(
                perp.segment.coord(participant_component_scores$COMPONENT_1, 
                participant_component_scores$COMPONENT_2, 
                0, 
                0)), 
               aes(x = x0, y = y0, xend = x1, yend = y1), alpha = 0) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  xlab(paste0("Component 1. Explained variance: ", round(pca_results$ExPosition.Data$t[1], digits=2),"%")) +
  ylab(paste0("Component 2. Explained variance: ", round(pca_results$ExPosition.Data$t[2], digits=2),"%")) +
  ggtitle("PCA of Trails & FDG:\nParticipant Component Scores") 
g

g_contributions <- g + 
  geom_segment(data=data.frame(x0=participant_component_scores$COMPONENT_1, 
                     y0=participant_component_scores$COMPONENT_2, 
                     x1=participant_component_scores$COMPONENT_1, 
                     y1=0), 
    aes(x = x0, y = y0, xend = x1, yend = y1), alpha = 1/3, color = "mediumorchid4")
g_contributions
g_contributions <- g_contributions + 
  geom_segment(data=data.frame(x0=participant_component_scores$COMPONENT_1, 
                      y0=participant_component_scores$COMPONENT_2, 
                      x1=0, 
                      y1=participant_component_scores$COMPONENT_2), 
    aes(x = x0, y = y0, xend = x1, yend = y1), alpha = 1/3, color = "olivedrab4")
g_contributions

## but what *is* Component 1? and Component 2?
  ## show the table here. explain that this is the last time we'll look at the table.
  ## which is "old school"
  ## actually, use the correlation circle. this is a fantastic approach.
    ## I can fall back on factoextra for that.

## what about a generalized package?
# fviz_pca(pca_results) +
#   theme_minimal() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank()) +
#   xlab(paste0("Component 1. Explained variance: ", round(pca_results$ExPosition.Data$t[1], digits=2),"%")) +
#   ylab(paste0("Component 2. Explained variance: ", round(pca_results$ExPosition.Data$t[2], digits=2),"%")) +
#   ggtitle("PCA of Trails & FDG:\nParticipant Component Scores")

var_cors <- fviz_pca_var(pca_results) +
  xlab(paste0("Component 1. Explained variance: ", round(pca_results$ExPosition.Data$t[1], digits=2),"%")) +
  ylab(paste0("Component 2. Explained variance: ", round(pca_results$ExPosition.Data$t[2], digits=2),"%")) +
  ggtitle("PCA:\nVariable-Component Correlations")

var_cors

## we won't be looking at tables or "biplots" any further.
  ## these only work in very small cases. 
  ## everything will be visualized from here on out and separately.
  ## use the min/max limits and make it an equal sized image
  ## why? because this is just too simple/easy of an example.

## second (usually this is first): how to do this with the package's utilities.
### the ExPosition way
  ## lots of graphs
# epGraphs(pca_results, contributionPlots = F)
#   ## individual graphs
# prettyPlot(pca_results$ExPosition.Data$fi)
# prettyPlot(pca_results$ExPosition.Data$fj)



