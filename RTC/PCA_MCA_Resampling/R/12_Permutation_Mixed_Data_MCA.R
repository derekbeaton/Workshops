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
permuted_eigenvalues <- matrix(NA, iterations, length(ca_results$ExPosition.Data$eigs))
for(i in 1:iterations){
  
  permuted_mixed_data <- cbind(
                      escofier.coding(apply(continuous_data, 2, sample), scale=T),
                      thermometer.coding(apply(ordinal_data,2,sample)),
                      make.data.nominal(apply(categorical_data,2,sample)))
  
  permuted_ca_results <- epCA(permuted_mixed_data, graphs=F)
  
  permuted_eigenvalues[i,1:length(permuted_ca_results$ExPosition.Data$eigs)] <- permuted_ca_results$ExPosition.Data$eigs
  
}


p_values <- pmax(colSums(t(t(permuted_eigenvalues) > ca_results$ExPosition.Data$eigs)) / iterations,1/iterations)


hist(permuted_eigenvalues[,1], xlim=c(min(permuted_eigenvalues[,1]),max(c(permuted_eigenvalues[,1],ca_results$ExPosition.Data$eigs[1]))), xlab="", main="First Component Permutation Distribution", breaks=20)
abline(v=ca_results$ExPosition.Data$eigs[1], col="red", lwd=2, lty=2)

hist(permuted_eigenvalues[,2], xlim=c(min(permuted_eigenvalues[,2]),max(c(permuted_eigenvalues[,2],ca_results$ExPosition.Data$eigs[2]))), xlab="", main="Second Component Permutation Distribution", breaks=20)
abline(v=ca_results$ExPosition.Data$eigs[2], col="red", lwd=2, lty=2)

hist(permuted_eigenvalues[,3], xlim=c(min(permuted_eigenvalues[,3]),max(c(permuted_eigenvalues[,3],ca_results$ExPosition.Data$eigs[3]))), xlab="", main="Third Component Permutation Distribution", breaks=20)
abline(v=ca_results$ExPosition.Data$eigs[3], col="red", lwd=2, lty=2)

hist(permuted_eigenvalues[,5], xlim=c(min(permuted_eigenvalues[,5]),max(c(permuted_eigenvalues[,5],ca_results$ExPosition.Data$eigs[5]))), xlab="", main="Fifth Component Permutation Distribution", breaks=20)
abline(v=ca_results$ExPosition.Data$eigs[5], col="red", lwd=2, lty=2)

