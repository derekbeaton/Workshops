library(ggplot2)
library(ExPosition)
library(ours)
library(tidyverse)

# data
load(file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","amerge_subset.rda"))
load(file=paste0(Sys.getenv("ADNI_FOLDER"),"\\","variable_type_map.rda"))

## break data into its constituent types, and then bind it back together after transforms.
categorical_data <- amerge_subset[,which(variable_type_map[,"Categorical"]==1)]
ordinal_data <- amerge_subset[,which(variable_type_map[,"Ordinal"]==1 & variable_type_map[,"Categorical"]!=1)]
continuous_data <- amerge_subset[,which(variable_type_map[,"Continuous"]==1)]

not_categorical_data <- amerge_subset[,which(variable_type_map[,"Categorical"]!=1)]
rank_data <- apply(not_categorical_data, 2, rank, na.last="keep")
  ## show properties of rank data
  
  ## explain the caution here...


  ## next steps
continuous_escofier_data <- escofier.coding(continuous_data, scale=T)
ordinal_thermometer_data <- thermometer.coding(ordinal_data)
categorical_disjunctive_data <- make.data.nominal(categorical_data)

mixed_data <- cbind(continuous_escofier_data,
                    ordinal_thermometer_data,
                    categorical_disjunctive_data)

rank_escofier_data <- escofier.coding(rank_data, scale=T)
rank_mixed_data <- cbind(rank_escofier_data, categorical_disjunctive_data)


mixed_ca_results <- epCA(mixed_data, graphs=F)
rank_ca_results <- epCA(rank_mixed_data, graphs=F)

### show the results


## then show the relationshps
cor(mixed_ca_results$ExPosition.Data$fi, rank_ca_results$ExPosition.Data$fi)

