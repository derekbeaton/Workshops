library(ggplot2)
library(ExPosition)
library(ours)
library(tidyverse)
library(factoextra)

# data
load(file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","amerge_subset.rda"))
load(file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","variable_type_map.rda"))

ordinal_or_categorical_data <- amerge_subset[,which(variable_type_map[,"Ordinal"]==1 & variable_type_map[,"Categorical"]==1)]


ordinal_or_categorical_thermometer_data <- thermometer.coding(ordinal_or_categorical_data)
ordinal_or_categorical_disjunctive_data <- make.data.nominal(ordinal_or_categorical_data)
## a look at each table


thermometer_ca_results <- epCA(ordinal_or_categorical_thermometer_data, graphs=F)
# disjunctive_ca_results <- epCA(ordinal_or_categorical_disjunctive_data, graphs=F)
mca_results <- epMCA(ordinal_or_categorical_data, graphs=F)


fviz_screeplot(thermometer_ca_results, addlabels = TRUE) +
  ggtitle("CA (thermometer):\nScree plot")
fviz_screeplot(mca_results, addlabels = TRUE) +
  ggtitle("MCA (disjunctive):\nScree plot")


ca_columns <- fviz_ca_col(thermometer_ca_results) +
  coord_cartesian(xlim=c(thermometer_ca_results$Plotting.Data$constraints$minx, thermometer_ca_results$Plotting.Data$constraints$maxx), ylim = c(thermometer_ca_results$Plotting.Data$constraints$miny, thermometer_ca_results$Plotting.Data$constraints$maxy)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  xlab(paste0("Component 1. Explained variance: ", round(thermometer_ca_results$ExPosition.Data$t[1], digits=2),"%")) +
  ylab(paste0("Component 2. Explained variance: ", round(thermometer_ca_results$ExPosition.Data$t[2], digits=2),"%")) +
  ggtitle("CA (thermometer):\nVariable Component Scores")

ca_columns + 
  geom_segment(data=data.frame(x0=thermometer_ca_results$ExPosition.Data$fj[1,1], 
                               y0=thermometer_ca_results$ExPosition.Data$fj[1,2], 
                               x1=thermometer_ca_results$ExPosition.Data$fj[3,1], 
                               y1=thermometer_ca_results$ExPosition.Data$fj[3,2]), 
               aes(x = x0, y = y0, xend = x1, yend = y1), color="red") +
  geom_segment(data=data.frame(x0=thermometer_ca_results$ExPosition.Data$fj[2,1], 
                               y0=thermometer_ca_results$ExPosition.Data$fj[2,2], 
                               x1=thermometer_ca_results$ExPosition.Data$fj[4,1], 
                               y1=thermometer_ca_results$ExPosition.Data$fj[4,2]), 
               aes(x = x0, y = y0, xend = x1, yend = y1), color="red")

mca_columns <- fviz_mca_var(mca_results) +
  coord_cartesian(xlim=c(mca_results$Plotting.Data$constraints$minx, mca_results$Plotting.Data$constraints$maxx), ylim = c(mca_results$Plotting.Data$constraints$miny, mca_results$Plotting.Data$constraints$maxy)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  xlab(paste0("Component 1. Explained variance: ", round(mca_results$ExPosition.Data$t[1], digits=2),"%")) +
  ylab(paste0("Component 2. Explained variance: ", round(mca_results$ExPosition.Data$t[2], digits=2),"%")) +
  ggtitle("MCA (disjunctive):\nVariable Component Scores")

mca_columns

mca_columns +
  geom_segment(aes(x = mca_results$ExPosition.Data$fj[1,1], 
                   y = mca_results$ExPosition.Data$fj[1,2],
                   xend = mca_results$ExPosition.Data$fj[2,1],
                   yend = mca_results$ExPosition.Data$fj[2,2]),
               arrow=arrow(type="closed", angle=30, length = unit(0.5, "cm")), color="red")

mca_columns +
  geom_segment(aes(x = mca_results$ExPosition.Data$fj[1,1], 
                   y = mca_results$ExPosition.Data$fj[1,2],
                   xend = mca_results$ExPosition.Data$fj[2,1],
                   yend = mca_results$ExPosition.Data$fj[2,2]),
                  arrow=arrow(type="closed", angle=30, length = unit(0.5, "cm")), color="red") +
  geom_segment(aes(x = mca_results$ExPosition.Data$fj[2,1], 
                   y = mca_results$ExPosition.Data$fj[2,2],
                   xend = mca_results$ExPosition.Data$fj[3,1],
                   yend = mca_results$ExPosition.Data$fj[3,2]),
               arrow=arrow(type="closed", angle=30, length = unit(0.5, "cm")), color="red")


mca_columns +
  geom_segment(aes(x = mca_results$ExPosition.Data$fj[5,1], 
                   y = mca_results$ExPosition.Data$fj[5,2],
                   xend = mca_results$ExPosition.Data$fj[4,1],
                   yend = mca_results$ExPosition.Data$fj[4,2]),
               arrow=arrow(type="closed", angle=30, length = unit(0.5, "cm")), color="red")

mca_columns +
  geom_segment(aes(x = mca_results$ExPosition.Data$fj[5,1], 
                   y = mca_results$ExPosition.Data$fj[5,2],
                   xend = mca_results$ExPosition.Data$fj[4,1],
                   yend = mca_results$ExPosition.Data$fj[4,2]),
               arrow=arrow(type="closed", angle=30, length = unit(0.5, "cm")), color="red") +
  geom_segment(aes(x = mca_results$ExPosition.Data$fj[4,1], 
                   y = mca_results$ExPosition.Data$fj[4,2],
                   xend = mca_results$ExPosition.Data$fj[7,1],
                   yend = mca_results$ExPosition.Data$fj[7,2]),
               arrow=arrow(type="closed", angle=30, length = unit(0.5, "cm")), color="red")

mca_columns +
  geom_segment(aes(x = mca_results$ExPosition.Data$fj[5,1], 
                   y = mca_results$ExPosition.Data$fj[5,2],
                   xend = mca_results$ExPosition.Data$fj[4,1],
                   yend = mca_results$ExPosition.Data$fj[4,2]),
               arrow=arrow(type="closed", angle=30, length = unit(0.5, "cm")), color="red") +
  geom_segment(aes(x = mca_results$ExPosition.Data$fj[4,1], 
                   y = mca_results$ExPosition.Data$fj[4,2],
                   xend = mca_results$ExPosition.Data$fj[7,1],
                   yend = mca_results$ExPosition.Data$fj[7,2]),
               arrow=arrow(type="closed", angle=30, length = unit(0.5, "cm")), color="red") +
  geom_segment(aes(x = mca_results$ExPosition.Data$fj[7,1], 
                   y = mca_results$ExPosition.Data$fj[7,2],
                   xend = mca_results$ExPosition.Data$fj[6,1],
                   yend = mca_results$ExPosition.Data$fj[6,2]),
               arrow=arrow(type="closed", angle=30, length = unit(0.5, "cm")), color="red")



fviz_ca_row(thermometer_ca_results, alpha.ind = 1/3, label="none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  xlim(c(thermometer_ca_results$Plotting.Data$constraints$minx, thermometer_ca_results$Plotting.Data$constraints$maxx)) +
  ylim(c(thermometer_ca_results$Plotting.Data$constraints$miny, thermometer_ca_results$Plotting.Data$constraints$maxy)) +
  xlab(paste0("Component 1. Explained variance: ", round(thermometer_ca_results$ExPosition.Data$t[1], digits=2),"%")) +
  ylab(paste0("Component 2. Explained variance: ", round(thermometer_ca_results$ExPosition.Data$t[2], digits=2),"%")) +
  ggtitle("CA (thermometer):\nParticipants Component Scores")


fviz_mca_ind(mca_results, alpha.ind = 1/3, label="none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  xlim(c(mca_results$Plotting.Data$constraints$minx, mca_results$Plotting.Data$constraints$maxx)) +
  ylim(c(mca_results$Plotting.Data$constraints$miny, mca_results$Plotting.Data$constraints$maxy)) +
  xlab(paste0("Component 1. Explained variance: ", round(mca_results$ExPosition.Data$t[1], digits=2),"%")) +
  ylab(paste0("Component 2. Explained variance: ", round(mca_results$ExPosition.Data$t[2], digits=2),"%")) +
  ggtitle("MCA (disjunctive):\nParticipants Component Scores")


cor(mca_results$ExPosition.Data$fi, thermometer_ca_results$ExPosition.Data$fi, method="spearman")


