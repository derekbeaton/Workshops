## quick & dirty mix

continuous_ordinal_data <- amerge_subset[,which(variable_type_map[,"Continuous"]==1 | variable_type_map[,"Ordinal"]==1 & variable_type_map[,"Categorical"]!=1)]
categorical_data <- amerge_subset[,which(variable_type_map[,"Ordinal"]!=1 & variable_type_map[,"Categorical"]==1)]

continuous_data <- amerge_subset[,which(variable_type_map[,"Continuous"]==1)]
categorical_ordinal_data <- amerge_subset[,which(variable_type_map[,"Ordinal"]!=1 | variable_type_map[,"Categorical"]==1)]



ca1 <- ca(cbind(escofier.coding(continuous_ordinal_data, scale=T), make.data.nominal(categorical_data)))
ca2 <- ca(cbind(escofier.coding(apply(continuous_ordinal_data, 2, rank), scale=T), make.data.nominal(categorical_data)))

ca3 <- ca(cbind(escofier.coding(continuous_data, scale=T), make.data.nominal(categorical_data)))
ca4 <- ca(cbind(escofier.coding(apply(continuous_data, 2, rank), scale=T), make.data.nominal(categorical_data)))



# binary example
amerge_subset %>%
  select(PTGENDER, PTETHCAT) ->
  binary_categorical_data
  
binary_categorical_data_as_disjunctive <- make.data.nominal(binary_categorical_data)

binary_categorical_data %>%
  mutate(PTGENDER = recode(PTGENDER, "Male"=0, "Female"=1)) %>%
  mutate(PTETHCAT = recode(PTETHCAT, "Not Hisp/Latino"=0, "Hisp/Latino"=1)) ->
binary_categorical_data_arbitrary_binarization 

  ## show the correlations and then show the plots.
  ## how we would interpret these
strict_binary_pca <- pca(binary_categorical_data_arbitrary_binarization)
binary_categories_ca <- ca(binary_categorical_data_as_disjunctive)

# escofier example
  ## a return to the first example

trails.fdg.df <- data.frame(TRAILS=amerge_subset$mPACCtrailsB, FDG=amerge_subset$FDG)
escofier_trails.fdg.df <- escofier.coding(trails.fdg.df, scale=T)

## show the correlations and then show the plots.
## how we would interpret these
standard_pca <- pca(trails.fdg.df)
escofier_ca <- pca(escofier_trails.fdg.df)  
  
