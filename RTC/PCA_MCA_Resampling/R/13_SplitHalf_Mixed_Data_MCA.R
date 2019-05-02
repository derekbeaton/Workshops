library(ggplot2)
library(ExPosition)
library(ours)
library(tidyverse)
library(factoextra)

# data
load(file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","amerge_subset.rda"))
load(file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","variable_type_map.rda"))

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

sh1_correlations <- sh2_correlations <- array(NA, dim=c(length(ca_results$ExPosition.Data$eigs),length(ca_results$ExPosition.Data$eigs), iterations) )


for(i in 1:iterations){
  
  sh1_sample <- sample(1:nrow(mixed_data), nrow(mixed_data)/2)
  sh2_sample <- sort(setdiff(1:nrow(mixed_data),sh1_sample))

  sh1_ca_results <- epCA(mixed_data[sh1_sample,], graphs=F)
  sh2_ca_results <- epCA(mixed_data[sh2_sample,], graphs=F)
  
  sh1_predict_from_sh2_results <- supplementaryRows(mixed_data[sh1_sample,], sh2_ca_results)
  sh2_predict_from_sh1_results <- supplementaryRows(mixed_data[sh2_sample,], sh1_ca_results)
  
  sh1_cors <- cor(sh1_predict_from_sh2_results$fii, sh1_ca_results$ExPosition.Data$fi)
  sh2_cors <- cor(sh2_predict_from_sh1_results$fii, sh2_ca_results$ExPosition.Data$fi)
  
  sh1_correlations[1:min(nrow(sh1_cors), nrow(sh1_correlations)), 1:min(ncol(sh1_cors), ncol(sh1_correlations)),i] <- sh1_cors
  sh2_correlations[1:min(nrow(sh2_cors), nrow(sh2_correlations)), 1:min(ncol(sh2_cors), ncol(sh2_correlations)),i] <- sh2_cors
  
}

average_sh_cors <- (abs(sh1_correlations) + abs(sh2_correlations)) / 2

corrplot::corrplot(apply(average_sh_cors,c(1,2),median), cl.lim = c(0,1), method="number")


