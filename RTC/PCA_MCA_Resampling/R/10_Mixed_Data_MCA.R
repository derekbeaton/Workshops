library(ggplot2)
library(ExPosition)
library(ours)
library(tidyverse)
library(factoextra)

# data
load(file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","amerge_subset.rda"))
load(file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","variable_type_map.rda"))

## break data into its constituent types, and then bind it back together after transforms.
continuous_data <- amerge_subset[,which(variable_type_map[,"Continuous"]==1)]
ordinal_data <- amerge_subset[,which(variable_type_map[,"Ordinal"]==1 & variable_type_map[,"Categorical"]!=1)]
categorical_data <- amerge_subset[,which(variable_type_map[,"Categorical"]==1)]
  ## let's drop DX from the analysis...
  categorical_data <- categorical_data[,-c(which("DX" %in% colnames(categorical_data)))]


continuous_escofier_data <- escofier.coding(continuous_data, scale=T)
ordinal_thermometer_data <- thermometer.coding(ordinal_data)
categorical_disjunctive_data <- make.data.nominal(categorical_data)

## show properties of the tables.

mixed_data <- cbind(continuous_escofier_data,
                    ordinal_thermometer_data,
                    categorical_disjunctive_data)

ca_results <- epCA(mixed_data, DESIGN = amerge_subset$DX, make_design_nominal = T, graphs=F)

fviz_screeplot(ca_results, addlabels = TRUE)

plus_minus <- ifelse(grepl("\\-",rownames(ca_results$ExPosition.Data$fj)),"-","+")
fviz_ca_col(ca_results, alpha=0, labels=F, col.col = "white") +
  geom_text(aes(label=rownames(ca_results$ExPosition.Data$fj), color=plus_minus)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none") +
  xlim(c(ca_results$Plotting.Data$constraints$minx, ca_results$Plotting.Data$constraints$maxx)) +
  ylim(c(ca_results$Plotting.Data$constraints$miny, ca_results$Plotting.Data$constraints$maxy)) +
  xlab(paste0("Component 1. Explained variance: ", round(ca_results$ExPosition.Data$t[1], digits=2),"%")) +
  ylab(paste0("Component 2. Explained variance: ", round(ca_results$ExPosition.Data$t[2], digits=2),"%")) +
  ggtitle("CA:\nVariable Component Scores")

# fviz_ca_row(ca_results, label="none") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank()) +
#   xlim(c(ca_results$Plotting.Data$constraints$minx, ca_results$Plotting.Data$constraints$maxx)) +
#   ylim(c(ca_results$Plotting.Data$constraints$miny, ca_results$Plotting.Data$constraints$maxy)) +
#   xlab(paste0("Component 1. Explained variance: ", round(ca_results$ExPosition.Data$t[1], digits=2),"%")) +
#   ylab(paste0("Component 2. Explained variance: ", round(ca_results$ExPosition.Data$t[2], digits=2),"%")) +
#   ggtitle("CA:\nParticipants Component Scores")



fviz_ca_row(ca_results, alpha=0, labels=F, col.row = "white") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank()) +
  geom_point(aes(x=ca_results$ExPosition.Data$fi[,1], y = ca_results$ExPosition.Data$fi[,2], color=amerge_subset$DX)) + 
  xlim(c(ca_results$Plotting.Data$constraints$minx, ca_results$Plotting.Data$constraints$maxx)) +
  ylim(c(ca_results$Plotting.Data$constraints$miny, ca_results$Plotting.Data$constraints$maxy)) +
  xlab(paste0("Component 1. Explained variance: ", round(ca_results$ExPosition.Data$t[1], digits=2),"%")) +
  ylab(paste0("Component 2. Explained variance: ", round(ca_results$ExPosition.Data$t[2], digits=2),"%")) +
  ggtitle("CA:\nParticipants Component Scores")
  