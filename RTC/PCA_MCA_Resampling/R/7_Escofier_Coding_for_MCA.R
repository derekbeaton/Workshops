library(ggplot2)
library(ExPosition)
library(ours)
library(tidyverse)
library(factoextra)

# data
load(file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","amerge_subset.rda"))
load(file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","variable_type_map.rda"))

## back to our first example.
amerge_subset %>%
  select(mPACCtrailsB, FDG) ->
  trails_fdg_data

## give some prep & set up
  ## say that there is a way to analyze continuous data but with CA (like MCA)
  ## show the properties of the table.
  ## let's see what happens

trails_fdg_escofier_data <- escofier.coding(trails_fdg_data,scale=T)
## show Escofier coded data
trails_fdg_escofier_data
  ## discuss the properties of these data vs. disjunctive. Oh hey!

  ## a caveat here: we have to do plain CA. It corrects itself... sort of. It's magic.
pca_results <- epPCA(trails_fdg_data, graphs=F)
ca_results <- epCA(trails_fdg_escofier_data, graphs=F)


fviz_pca_var(pca_results) +
  xlab(paste0("Component 1. Explained variance: ", round(pca_results$ExPosition.Data$t[1], digits=2),"%")) +
  ylab(paste0("Component 2. Explained variance: ", round(pca_results$ExPosition.Data$t[2], digits=2),"%")) +
  ggtitle("PCA:\nVariable-Component Correlations")

ca_columns <- fviz_ca_col(ca_results) +
  coord_cartesian(xlim=c(ca_results$Plotting.Data$constraints$minx, ca_results$Plotting.Data$constraints$maxx), ylim = c(ca_results$Plotting.Data$constraints$miny, ca_results$Plotting.Data$constraints$maxy)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  xlab(paste0("Component 1. Explained variance: ", round(ca_results$ExPosition.Data$t[1], digits=2),"%")) +
  ylab(paste0("Component 2. Explained variance: ", round(ca_results$ExPosition.Data$t[2], digits=2),"%")) +
  ggtitle("CA:\nVariable Component Scores")
ca_columns

ca_columns + 
  geom_segment(data=data.frame(x0=ca_results$ExPosition.Data$fj[1,1], 
                               y0=ca_results$ExPosition.Data$fj[1,2], 
                               x1=ca_results$ExPosition.Data$fj[3,1], 
                               y1=ca_results$ExPosition.Data$fj[3,2]), 
               aes(x = x0, y = y0, xend = x1, yend = y1), color="red") +
  geom_segment(data=data.frame(x0=ca_results$ExPosition.Data$fj[2,1], 
                               y0=ca_results$ExPosition.Data$fj[2,2], 
                               x1=ca_results$ExPosition.Data$fj[4,1], 
                               y1=ca_results$ExPosition.Data$fj[4,2]), 
               aes(x = x0, y = y0, xend = x1, yend = y1), color="red")


## have to slightly emphasize here the doubly-coded columns and how to interpret this.
  ## we'd interpret the same way.

fviz_screeplot(pca_results, addlabels = TRUE) +
  ggtitle("PCA:\nScree plot")
fviz_screeplot(ca_results, addlabels = TRUE) +
  ggtitle("CA:\nScree plot")


fviz_pca_ind(pca_results, alpha.ind = 1/3, label="none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  xlab(paste0("Component 1. Explained variance: ", round(pca_results$ExPosition.Data$t[1], digits=2),"%")) +
  ylab(paste0("Component 2. Explained variance: ", round(pca_results$ExPosition.Data$t[2], digits=2),"%")) +
  ggtitle("PCA:\nParticipants Component Scores")


fviz_ca_row(ca_results, alpha.ind = 1/3, label="none") +
  #coord_cartesian(xlim=c(ca_results$Plotting.Data$constraints$minx, ca_results$Plotting.Data$constraints$maxx), ylim = c(ca_results$Plotting.Data$constraints$miny, ca_results$Plotting.Data$constraints$maxy)) +
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


cor(pca_results$ExPosition.Data$fi, ca_results$ExPosition.Data$fi)
(pca_results$ExPosition.Data$eigs/ca_results$ExPosition.Data$eigs)

## so how can we make this easier? Let's only try to emphasize one side
## we will leave all the information, but just one side.
  ## show side by side with the correlation plot.

### now is time for a big breather.
  ## we just learned how to make continuous data look "categorical" without any loss of information!
  ### WOAH.
