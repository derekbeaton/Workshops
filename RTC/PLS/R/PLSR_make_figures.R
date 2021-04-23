### plsr figures


## for making figures

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


Y_scale <- scale(Y_numeric,T,T)

# gpls_plsr_res <- GPLS::pls_reg(X_numeric, Y_numeric, T, T, T, T)
plsr_res <- baby_plsr(X_numeric, Y_numeric, T, T, T, T)


predicted_Y_component_1 <- plsr_res$Y_reconstructed_array[,,1]
predicted_Y_component_2 <- plsr_res$Y_reconstructed_array[,,2]
predicted_Y_component_3 <- plsr_res$Y_reconstructed_array[,,3]

predicted_Y_component_1_and_2 <- plsr_res$Y_reconstructed_array[,,1] + plsr_res$Y_reconstructed_array[,,2]
predicted_Y_component_1_and_2_and_3 <- plsr_res$Y_reconstructed_array[,,1] + plsr_res$Y_reconstructed_array[,,2] + plsr_res$Y_reconstructed_array[,,3]

## to help verify the additivity:
# predicted_Y_component_1_and_2_and_3 / plsr_res$Y_reconstructed



all_them_Ys <- data.frame(
  original_Y = I(Y_scale),
  predicted_Y_component_1 = I(predicted_Y_component_1),
  predicted_Y_component_2 = I(predicted_Y_component_2),
  predicted_Y_component_3 = I(predicted_Y_component_3),
  predicted_Y_component_1_and_2 = I(predicted_Y_component_1_and_2),
  predicted_Y_component_1_and_2_and_3 = I(predicted_Y_component_1_and_2_and_3)
)


## for C1
for(i in 1:ncol(Y_scale)){
  
  all_them_Ys %>%
    ggplot(., aes(x = original_Y[,i], y = predicted_Y_component_1[,i])) +
    geom_point() +
    theme_bw() +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    xlab(paste("Original: ",colnames(all_them_Ys$original_Y)[i])) +
    ylab(paste("Predicted: ",colnames(all_them_Ys$original_Y)[i])) +
    ggtitle("Component (Latent Variable) 1.") -> p
  print(p)

}



## for C2
for(i in 1:ncol(Y_scale)){
  
  all_them_Ys %>%
    ggplot(., aes(x = original_Y[,i], y = predicted_Y_component_2[,i])) +
    geom_point() +
    theme_bw() +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    xlab(paste("Original: ",colnames(all_them_Ys$original_Y)[i])) +
    ylab(paste("Predicted: ",colnames(all_them_Ys$original_Y)[i])) +
    ggtitle("Component (Latent Variable) 2.") -> p
  print(p)
  
}


## for C1 + C2
for(i in 1:ncol(Y_scale)){
  
  all_them_Ys %>%
    ggplot(., aes(x = original_Y[,i], y = predicted_Y_component_1_and_2[,i])) +
    geom_point() +
    theme_bw() +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    xlab(paste("Original: ",colnames(all_them_Ys$original_Y)[i])) +
    ylab(paste("Predicted: ",colnames(all_them_Ys$original_Y)[i])) +
    ggtitle("Components (Latent Variables) 1 + 2.") -> p
  print(p)
  
}


## for C1 + C2 + C3
for(i in 1:ncol(Y_scale)){
  
  all_them_Ys %>%
    ggplot(., aes(x = original_Y[,i], y = predicted_Y_component_1_and_2_and_3[,i])) +
    geom_point() +
    theme_bw() +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    xlab(paste("Original: ",colnames(all_them_Ys$original_Y)[i])) +
    ylab(paste("Predicted: ",colnames(all_them_Ys$original_Y)[i])) +
    ggtitle("Components (Latent Variables) 1 + 2 + 3.") -> p
  print(p)
  
}



### an example of C1, C1 + C2, C1 + C2 + C3 for fitted vs. residual
  ### alternatively, I am using the reconstructed because I reserve the fitted to be the re-centered and re-scaled
# 

Stroop_switch_fitted_residuals_df <- data.frame(
  fitted_c1 = plsr_res$Y_reconstructed_array[,6,1],
  fitted_c1_and_c2 = plsr_res$Y_reconstructed_array[,6,1] + plsr_res$Y_reconstructed_array[,6,2],
  fitted_c1_and_c2_and_c3 = plsr_res$Y_reconstructed_array[,6,1] + plsr_res$Y_reconstructed_array[,6,2]  + plsr_res$Y_reconstructed_array[,6,3],
  residual_c1 = plsr_res$Y_residual_array[,6,1],
  residual_c1_and_c2 = plsr_res$Y_residual_array[,6,1] + plsr_res$Y_residual_array[,6,2],
  residual_c1_and_c2_and_c3 = plsr_res$Y_residual_array[,6,1] + plsr_res$Y_residual_array[,6,2]  + plsr_res$Y_residual_array[,6,3]
)


Stroop_switch_fitted_residuals_df %>%
  ggplot(., aes(x = fitted_c1, y = residual_c1)) +
    geom_point() +
    theme_bw() +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    xlab("Fitted Stroop switch: Component 1") +
    ylab("Residual Stroop switch: Component 1") +
    ggtitle("Fitted vs. Residual: Component 1") 


Stroop_switch_fitted_residuals_df %>%
  ggplot(., aes(x = fitted_c1_and_c2, y = residual_c1_and_c2)) +
  geom_point() +
  theme_bw() +
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()) +
  xlab("Fitted Stroop switch: Components 1 + 2") +
  ylab("Residual Stroop switch: Components 1 + 2") +
  ggtitle("Fitted vs. Residual: Components 1 + 2") 


Stroop_switch_fitted_residuals_df %>%
  ggplot(., aes(x = fitted_c1_and_c2_and_c3, y = residual_c1_and_c2_and_c3)) +
  geom_point() +
  theme_bw() +
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()) +
  xlab("Fitted Stroop switch: Components 1 + 2 + 3") +
  ylab("Residual Stroop switch: Components 1 + 2 + 3") +
  ggtitle("Fitted vs. Residual: Components 1 + 2 + 3") 




lm_res <- lm(as.matrix(Y_numeric) ~ as.matrix(X_numeric))


Stroop_switch_fitted_residuals_pls_and_lm <- data.frame(
  fitted_plsr = plsr_res$Y_fitted_values[,6],
  residual_plsr = plsr_res$Y_residuals[,6],
  fitted_lm = lm_res$fitted.values[,6],
  residual_lm = lm_res$residuals[,6]
)


Stroop_switch_fitted_residuals_pls_and_lm %>%
  ggplot(., aes(x = fitted_plsr, y = fitted_lm)) +
  geom_point() +
  theme_bw() +
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()) +
  xlab("Fitted Stroop switch: PLSR Components 1 + 2 + 3") +
  ylab("Fitted Stroop switch: lm()") +
  ggtitle("Fitted: PLSR vs. lm()") 


Stroop_switch_fitted_residuals_pls_and_lm %>%
  ggplot(., aes(x = residual_plsr, y = residual_lm)) +
  geom_point() +
  theme_bw() +
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()) +
  xlab("Residual Stroop switch: PLSR Components 1 + 2 + 3") +
  ylab("Residual Stroop switch: lm()") +
  ggtitle("Residuals: PLSR vs. lm()") 
