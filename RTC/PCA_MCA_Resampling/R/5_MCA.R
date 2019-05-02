library(ggplot2)
library(ExPosition)
library(factoextra)

# data
load(file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","amerge_subset.rda"))
load(file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","variable_type_map.rda"))

# select the (mostly strictly) categorical subset + ApoE (a possibly ordinal)
categorical_data_subset <- amerge_subset[,which(variable_type_map[,"Categorical"]==1)]

mca_results <- epMCA(categorical_data_subset, graphs=F)

## look at the table:
# categorical_data_subset

fviz_screeplot(mca_results, addlabels=TRUE)

fviz_mca_var(mca_results, repel = T) +
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

