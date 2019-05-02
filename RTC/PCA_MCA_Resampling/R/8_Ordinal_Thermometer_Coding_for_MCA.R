library(ggplot2)
library(ExPosition)
library(ours)
library(tidyverse)
library(factoextra)

# data
load(file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","amerge_subset.rda"))
load(file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","variable_type_map.rda"))

strictly_ordinal_data <- amerge_subset[,which(variable_type_map[,"Ordinal"]==1 & variable_type_map[,"Categorical"]!=1)]

strictly_ordinal_thermometer_data <- thermometer.coding(strictly_ordinal_data)

## show the properties of the table
strictly_ordinal_thermometer_data
  ## well look at that.

ca_results <- epCA(strictly_ordinal_thermometer_data, graphs=F)


fviz_screeplot(ca_results, addlabels = TRUE) +
  ggtitle("CA:\nScree plot")


## have to majorly emphasize here the doubly-coded columns and how to interpret this.
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
                               x1=ca_results$ExPosition.Data$fj[5,1], 
                               y1=ca_results$ExPosition.Data$fj[5,2]), 
               aes(x = x0, y = y0, xend = x1, yend = y1), color="red") +
  geom_segment(data=data.frame(x0=ca_results$ExPosition.Data$fj[2,1], 
                               y0=ca_results$ExPosition.Data$fj[2,2], 
                               x1=ca_results$ExPosition.Data$fj[6,1], 
                               y1=ca_results$ExPosition.Data$fj[6,2]), 
               aes(x = x0, y = y0, xend = x1, yend = y1), color="red") + 
  geom_segment(data=data.frame(x0=ca_results$ExPosition.Data$fj[3,1], 
                               y0=ca_results$ExPosition.Data$fj[3,2], 
                               x1=ca_results$ExPosition.Data$fj[7,1], 
                               y1=ca_results$ExPosition.Data$fj[7,2]), 
               aes(x = x0, y = y0, xend = x1, yend = y1), color="red") +
  geom_segment(data=data.frame(x0=ca_results$ExPosition.Data$fj[4,1], 
                               y0=ca_results$ExPosition.Data$fj[4,2], 
                               x1=ca_results$ExPosition.Data$fj[8,1], 
                               y1=ca_results$ExPosition.Data$fj[8,2]), 
               aes(x = x0, y = y0, xend = x1, yend = y1), color="red")


plus_minus <- ifelse(grepl("\\+",rownames(ca_results$ExPosition.Data$fj)),"+","-")
fviz_ca_col(ca_results, alpha.col = 0, labels=F, col.col = "white") +
  #geom_point(aes(x=ca_results$ExPosition.Data$fj[,1], y = ca_results$ExPosition.Data$fj[,2], color=plus_minus)) +
  geom_text(aes(label=rownames(ca_results$ExPosition.Data$fj), color=plus_minus)) +
  coord_cartesian(xlim=c(ca_results$Plotting.Data$constraints$minx, ca_results$Plotting.Data$constraints$maxx), ylim = c(ca_results$Plotting.Data$constraints$miny, ca_results$Plotting.Data$constraints$maxy)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none") +
  xlab(paste0("Component 1. Explained variance: ", round(ca_results$ExPosition.Data$t[1], digits=2),"%")) +
  ylab(paste0("Component 2. Explained variance: ", round(ca_results$ExPosition.Data$t[2], digits=2),"%")) +
  ggtitle("CA:\nVariable Component Scores")

## have to slightly emphasize here the doubly-coded columns and how to interpret this.
## we'd interpret the same way.

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



## so how can we make this easier? Let's only try to emphasize one side
  ## we will leave all the information, but just one side.
  ## show side by side with the correlation plot.

## another big breather.
  ## we now know how to deal with ordinal data
  ## let's take a look at the three types of data tables formatted for use with CA.

