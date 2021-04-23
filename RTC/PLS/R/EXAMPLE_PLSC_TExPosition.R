# PLSC via TExPosition and resampling via TInPosition

source('R/_libraries.R')

TOY_DATA <- read.csv("./data/OND01_TOY_DATA.csv")

TOY_DATA %>%
  drop_na ->
  TOY_DATA_COMPLETE_CASES


TOY_DATA_COMPLETE_CASES %>%
  select(SUBJECT, AGE, NAGM_PERCENT, NAWM_PERCENT) %>%
  tibble::column_to_rownames(.,"SUBJECT") ->
  X_numeric


TOY_DATA_COMPLETE_CASES %>%
  select(SUBJECT, starts_with("TMT"), starts_with("Stroop")) %>%
  tibble::column_to_rownames(.,"SUBJECT") ->
  Y_numeric




### plain PLSC

plsc_res <- tepPLS(X_numeric, Y_numeric, graphs = F)
  ## to see all the graphs at once, use this:
# tepGraphs(plsc_res)


 
iterations <- 100

permuted_eigenvalues <- matrix(NA,iterations,length(plsc_res$TExPosition.Data$eigs))


bootstrap_row_scores_cube <- array(NA, dim=c(ncol(X_numeric), length(plsc_res$TExPosition.Data$eigs), iterations))
bootstrap_column_scores_cube <- array(NA, dim=c(ncol(Y_numeric), length(plsc_res$TExPosition.Data$eigs), iterations))

rownames(bootstrap_row_scores_cube) <- colnames(X_numeric)
rownames(bootstrap_column_scores_cube) <- colnames(Y_numeric)




for(i in 1:iterations){

  ### permutation
  perm_sample <- sample(1:nrow(X_numeric))
  perm_plsc_res <- tepPLS(X_numeric[perm_sample,], Y_numeric, graphs = F)
  permuted_eigenvalues[i,] <- perm_plsc_res$TExPosition.Data$eigs
                        

  ### bootstrap

  boot_sample <- sample(1:nrow(X_numeric), replace = T)

  X_boot <- scale(X_numeric[boot_sample,], 
                  center = plsc_res$TExPosition.Data$data1.norm$center,
                  scale = plsc_res$TExPosition.Data$data1.norm$scale)
  
  Y_boot <- scale(Y_numeric[boot_sample,], 
                  center = plsc_res$TExPosition.Data$data2.norm$center,
                  scale = plsc_res$TExPosition.Data$data2.norm$scale)


  bootstrap_row_scores_cube[,,i] <- t(X_boot) %*% plsc_res$TExPosition.Data$ly[boot_sample,]
  bootstrap_column_scores_cube[,,i] <- t(Y_boot) %*% plsc_res$TExPosition.Data$lx[boot_sample,]

  print(i)
}


# permutation-based p-values for components
boolean_bigger_eigs <- sweep(permuted_eigenvalues,2,STATS = plsc_res$TExPosition.Data$eigs,">")
p_values <- colSums(boolean_bigger_eigs) / iterations
  ## NOTE: No p-value is ever 0, it just means it's p < (1/iterations)


# bootstrap ratios (akin to z scores) for identifying stable variables (e.g., those with BSR > 2 are ~ p < .05)
bootstrap_ratios_rows <- boot.ratio.test(bootstrap_row_scores_cube)
bootstrap_ratios_columns <- boot.ratio.test(bootstrap_column_scores_cube)


