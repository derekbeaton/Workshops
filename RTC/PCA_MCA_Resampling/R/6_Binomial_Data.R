library(ggplot2)
library(ExPosition)
library(tidyverse)

# data
load(file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","amerge_subset.rda"))
load(file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","variable_type_map.rda"))

# select specific categorical data with only two levels per variable.
gender_ethnicity_data <- amerge_subset[,c("PTGENDER","PTETHCAT")]
gender_ethnicity_disjunctive_data <- makeNominalData(gender_ethnicity_data)

gender_ethnicity_binary_data <- gender_ethnicity_data
gender_ethnicity_binary_data$PTGENDER <- recode(gender_ethnicity_binary_data$PTGENDER, Male= 0,  Female = 1)
gender_ethnicity_binary_data$PTETHCAT <- recode(gender_ethnicity_binary_data$PTETHCAT, `Hisp/Latino` = 1,  `Not Hisp/Latino` = 0)


## these are equal (should be shown side-by-side).
gender_ethnicity_binary_data == gender_ethnicity_disjunctive_data[,c(2,4)]


## talk about the properties of each table...
gender_ethnicity_disjunctive_data
gender_ethnicity_binary_data


mca_results <- epMCA(gender_ethnicity_data, graphs=F)
pca_results <- epPCA(gender_ethnicity_binary_data, graphs=F)

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


fviz_pca_var(pca_results) +
  xlab(paste0("Component 1. Explained variance: ", round(pca_results$ExPosition.Data$t[1], digits=2),"%")) +
  ylab(paste0("Component 2. Explained variance: ", round(pca_results$ExPosition.Data$t[2], digits=2),"%")) +
  ggtitle("PCA:\nVariable-Component Correlations")



fviz_screeplot(mca_results, addlabels = TRUE) +
  ggtitle("MCA:\nScree plot")
fviz_screeplot(pca_results, addlabels = TRUE) +
  ggtitle("PCA:\nScree plot")


fviz_mca_ind(mca_results, alpha.ind = 1/3, label="none") +
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


fviz_pca_ind(pca_results, alpha.ind = 1/3, label="none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  xlab(paste0("Component 1. Explained variance: ", round(pca_results$ExPosition.Data$t[1], digits=2),"%")) +
  ylab(paste0("Component 2. Explained variance: ", round(pca_results$ExPosition.Data$t[2], digits=2),"%")) +
  ggtitle("PCA:\nParticipants Component Scores")


cor(mca_results$ExPosition.Data$fi, pca_results$ExPosition.Data$fi)

## bring the discussion about flips in here.
## also point out how with binomial data, you'll get a lot more information out of MCA than PCA.

## now allows me to make a point about the next data:
  ### there's something very special about two columns.