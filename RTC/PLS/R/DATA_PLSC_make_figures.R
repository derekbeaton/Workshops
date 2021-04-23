
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


  X_scale <- scale(X_numeric)
  Y_scale <- scale(Y_numeric)
  R <- t(X_scale) %*% Y_scale
  
  
  ### have to fix the colors so that they use + & -
  
  X_scale_breaks <- c(seq(min(X_scale),0,length.out = 51),seq(max(X_scale)/100,max(X_scale),length.out = 50))
  Y_scale_breaks <- c(seq(min(X_scale),0,length.out = 51),seq(max(X_scale)/100,max(X_scale),length.out = 50))
  R_breaks <- c(seq(min(R),0,length.out = 51),seq(max(R)/100,max(R),length.out = 50))
  
  
  pheatmap::pheatmap(X_scale, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, labels_row = "", breaks = X_scale_breaks)
  pheatmap::pheatmap(Y_scale, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, labels_row = "", breaks = Y_scale_breaks)
  pheatmap::pheatmap(R, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, breaks = R_breaks)
  
  pheatmap::pheatmap(R, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, breaks = R_breaks, labels_row = "", labels_col = "")
  
  
  svd_res <- svd(R)
 
  U_breaks <- c(seq(min(svd_res$u),0,length.out = 51),seq(max(svd_res$u)/100,max(svd_res$u),length.out = 50))
  V_breaks <- c(seq(min(svd_res$v),0,length.out = 51),seq(max(svd_res$v)/100,max(svd_res$v),length.out = 50))
  
  pheatmap::pheatmap(svd_res$u, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, breaks = U_breaks)
  pheatmap::pheatmap(svd_res$v, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, breaks = V_breaks)
  
  na_mat <- matrix(NA,length(svd_res$d), length(svd_res$d))
  diag(na_mat) <- svd_res$d
  pheatmap::pheatmap(na_mat, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F)
  pheatmap::pheatmap(na_mat^2, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F)
  
  
  FJ <- svd_res$u %*% diag(svd_res$d)
  FK <- svd_res$v %*% diag(svd_res$d)
  FJ_breaks <- c(seq(min(FJ),0,length.out = 51),seq(max(FJ)/100,max(FJ),length.out = 50))
  FK_breaks <- c(seq(min(FK),0,length.out = 51),seq(max(FK)/100,max(FK),length.out = 50))
  
  pheatmap::pheatmap(FJ, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, breaks = FJ_breaks)
  pheatmap::pheatmap(FK, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, breaks = FK_breaks)
  
  pheatmap::pheatmap(FJ, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, breaks = FJ_breaks, display_numbers = T, fontsize_number = 14, number_color = "white")
  pheatmap::pheatmap(FK, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, breaks = FK_breaks, display_numbers = T, fontsize_number = 14, number_color = "white")
  
  
  
  viz_comp1_u <- svd_res$u
  viz_comp1_u[,c(2:3)] <- NA
  
  viz_comp2_u <- svd_res$u
  viz_comp2_u[,c(1,3)] <- NA
  
  viz_comp3_u <- svd_res$u
  viz_comp3_u[,c(1:2)] <- NA
  
  viz_comp1_v <- svd_res$v
  viz_comp1_v[,c(2:3)] <- NA
  
  viz_comp2_v <- svd_res$v
  viz_comp2_v[,c(1,3)] <- NA
  
  viz_comp3_v <- svd_res$v
  viz_comp3_v[,c(1:2)] <- NA
  
  viz_d_1 <- viz_d_2 <- viz_d_3 <- matrix(NA,length(svd_res$d), length(svd_res$d))
  viz_d_1[1,1] <- svd_res$d[1]
  viz_d_2[2,2] <- svd_res$d[2]
  viz_d_3[3,3] <- svd_res$d[3]
  
  
  rank1_comp1 <- svd_res$u[,1,drop=F] %*% svd_res$d[1] %*% t(svd_res$v[,1,drop=F])
  rank1_comp2 <- svd_res$u[,2,drop=F] %*% svd_res$d[2] %*% t(svd_res$v[,2,drop=F])
  rank1_comp3 <- svd_res$u[,3,drop=F] %*% svd_res$d[3] %*% t(svd_res$v[,3,drop=F])
  
  r1c1_breaks <- c(seq(min(R),0,length.out = 51),seq(max(R)/100,max(R),length.out = 50))
  r1c2_breaks <- c(seq(min(R),0,length.out = 51),seq(max(R)/100,max(R),length.out = 50))
  r1c3_breaks <- c(seq(min(R),0,length.out = 51),seq(max(R)/100,max(R),length.out = 50))
  
  d_breaks <- c()
  
  
  pheatmap::pheatmap(viz_comp1_u, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, breaks = U_breaks)
  # pheatmap::pheatmap(viz_d_1, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F)
  pheatmap::pheatmap(viz_comp1_v, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, breaks = V_breaks)
  pheatmap::pheatmap(rank1_comp1, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, breaks = r1c1_breaks)
  
  
  pheatmap::pheatmap(viz_comp2_u, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, breaks = U_breaks)
  # pheatmap::pheatmap(viz_d_2, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F)
  pheatmap::pheatmap(viz_comp2_v, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, breaks = V_breaks)
  pheatmap::pheatmap(rank1_comp2, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, breaks = r1c2_breaks)
  

  pheatmap::pheatmap(viz_comp3_u, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, breaks = U_breaks)
  # pheatmap::pheatmap(viz_d_3, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F)
  pheatmap::pheatmap(viz_comp3_v, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, breaks = V_breaks)
  pheatmap::pheatmap(rank1_comp3, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, breaks = r1c3_breaks)
  
  
  
  
  ### some verification:
  rebuild_all_comps <- svd_res$u %*% diag(svd_res$d) %*% t(svd_res$v)
  rebuild_all_comps / R
  rebuild_all_comps / (rank1_comp1 + rank1_comp2 + rank1_comp3)
  
  ## et voila
  
  
  
  
  ## show component scores (just two axes)
  X_comp_scores <- svd_res$u %*% diag(svd_res$d)
    colnames(X_comp_scores) <- paste0("Component_",1:3)
    rownames(X_comp_scores) <- colnames(X_numeric)
  Y_comp_scores <- svd_res$v %*% diag(svd_res$d)
    colnames(Y_comp_scores) <- paste0("Component_",1:3)
    rownames(Y_comp_scores) <- colnames(Y_numeric)
    
  X_comp_scores %>%
    as.data.frame() %>%
    ggplot(., aes(x = Component_1, y = Component_2, label = rownames(.))) +
    geom_point(alpha = 0) +
    # coord_fixed(ratio = sqrt(svd_res$d[2] / svd_res$d[1])*5) +
    xlim(-max(abs(X_comp_scores[,1])),max(abs(X_comp_scores[,1]))) +
    ylim(-max(abs(X_comp_scores[,2])),max(abs(X_comp_scores[,2]))) +
    theme_minimal() +
    theme(
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) ->
    base_x_scores_plot
    
 
  base_x_scores_plot
     
  base_x_scores_plot +
    geom_point(aes(x = Component_1, y = 0), color = "firebrick3", size = 2)
    
 
  base_x_scores_plot +
    geom_point(aes(x = Component_1, y = 0), color = "firebrick3", size = 2) +
    #geom_text(aes(x = Component_1, y = 0), nudge_y = 5, angle = 30, color = "firebrick3")
    geom_text_repel(aes(x = Component_1, y = 0), color = "firebrick3")
  
  base_x_scores_plot +
    geom_point(aes(x = Component_1, y = Component_2), color = "firebrick3", size = 2)
  
  base_x_scores_plot +
    geom_point(aes(x = Component_1, y = Component_2), color = "firebrick3", size = 2) +
    geom_text_repel(aes(x = Component_1, y = Component_2), color = "firebrick3")
  
  
  
  
  X_comp_scores %>%
    as.data.frame() %>%
    ggplot(., aes(y = Component_1, x = Component_2, label = rownames(.))) +
    geom_point(alpha = 0) +
    # coord_fixed(ratio = sqrt(svd_res$d[2] / svd_res$d[1])*5) +
    xlim(-max(abs(X_comp_scores[,2])),max(abs(X_comp_scores[,2]))) +
    ylim(-max(abs(X_comp_scores[,1])),max(abs(X_comp_scores[,1]))) +
    theme_minimal() +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) ->
    base_x_scores_plot_flip
  
  base_x_scores_plot_flip +
    geom_point(aes(y = 0, x = Component_2), color = "firebrick3", size = 2) +
    geom_text_repel(aes(y = 0, x = Component_2), color = "firebrick3")
  
  
  
  
  
  
  
  
  Y_comp_scores %>%
    as.data.frame() %>%
    ggplot(., aes(x = Component_1, y = Component_2, label = rownames(.))) +
    geom_point(alpha = 0) +
    # coord_fixed(ratio = sqrt(svd_res$d[2] / svd_res$d[1])*2) +
    xlim(-max(abs(Y_comp_scores[,1])),max(abs(Y_comp_scores[,1]))) +
    ylim(-max(abs(Y_comp_scores[,2])),max(abs(Y_comp_scores[,2]))) +
    theme_minimal() +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) ->
    base_y_scores_plot
  
  
  base_y_scores_plot
  
  base_y_scores_plot +
    geom_point(aes(x = Component_1, y = 0), color = "firebrick3", size = 2)
  
  base_y_scores_plot +
    geom_point(aes(x = Component_1, y = 0), color = "firebrick3", size = 2) +
    # geom_text(aes(x = Component_1, y = 0), nudge_y = 5, angle = 30, color = "firebrick3")
    geom_text_repel(aes(x = Component_1, y = 0), color = "firebrick3")
  
  base_y_scores_plot +
    geom_point(aes(x = Component_1, y = Component_2), color = "firebrick3", size = 2)
  
  base_y_scores_plot +
    geom_point(aes(x = Component_1, y = Component_2), color = "firebrick3", size = 2) +
    geom_text_repel(aes(x = Component_1, y = Component_2), color = "firebrick3")
  
  
  base_y_scores_plot +
    geom_point(aes(x = 0, y = Component_2), color = "firebrick3", size = 2) +
    geom_text_repel(aes(x = 0, y = Component_2), color = "firebrick3")
  
  
  
  
  Y_comp_scores %>%
    as.data.frame() %>%
    ggplot(., aes(x = Component_2, y = Component_1, label = rownames(.))) +
    geom_point(alpha = 0) +
    # coord_fixed(ratio = sqrt(svd_res$d[2] / svd_res$d[1])*2) +
    xlim(-max(abs(Y_comp_scores[,2])),max(abs(Y_comp_scores[,2]))) +
    ylim(-max(abs(Y_comp_scores[,1])),max(abs(Y_comp_scores[,1]))) +
    theme_minimal() +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) ->
    base_y__flip
  
  
  
  base_y__flip +
    geom_point(aes(x = 0, y = Component_1), color = "firebrick3", size = 2) +
    # geom_text(aes(x = Component_1, y = 0), nudge_y = 5, angle = 30, color = "firebrick3")
    geom_text_repel(aes(y = Component_1, x = 0), color = "firebrick3")
  
  
  
  
  
  
  ### now latent variables
  LX <- X_scale %*% svd_res$u
  LY <- Y_scale %*% svd_res$v
  t(LX) %*% LY
  
  LX_breaks <- c(seq(min(LX),0,length.out = 51),seq(max(LX)/100,max(LX),length.out = 50))
  LY_breaks <- c(seq(min(LY),0,length.out = 51),seq(max(LY)/100,max(LY),length.out = 50))
  
  pheatmap::pheatmap(LX, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, breaks = LX_breaks, labels_row = "")
  pheatmap::pheatmap(LY, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, breaks = LY_breaks, labels_row = "")
  
  LX_1 <- LX
  LX_1[,c(2:3)] <- NA
  
  LX_2 <- LX
  LX_2[,c(1,3)] <- NA
  
  LY_1 <- LY
  LY_1[,c(2:3)] <- NA
  
  LY_2 <- LY
  LY_2[,c(1,3)] <- NA
  
  
  pheatmap::pheatmap(LX_1, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, breaks = LX_breaks, labels_row = "")
  pheatmap::pheatmap(LX_2, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, breaks = LX_breaks, labels_row = "")
  
  pheatmap::pheatmap(LY_1, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, breaks = LY_breaks, labels_row = "")
  pheatmap::pheatmap(LY_2, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F, breaks = LY_breaks, labels_row = "")
  
  
  ## this gets us our (variable) component scores back by projecting the data onto the opposite latent variables (e.g., project X onto LY)
  X_comp_scores_project <- t(X_scale) %*% LY
  Y_comp_scores_project <- t(Y_scale) %*% LX
  
  ## verify, et voila
  X_comp_scores_project / X_comp_scores
  Y_comp_scores_project / Y_comp_scores
  
  
  LVs_df <- data.frame(
    LX1 = LX[,1],
    LX2 = LX[,2],
    LY1 = LY[,1],
    LY2 = LY[,2]
  )
  
  LVs_df %>%
    ggplot(., aes(x = LX1, y = LY1)) +
    theme_minimal() +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) +
    geom_point(color = "mediumorchid4")
  
  
  LVs_df %>%
    ggplot(., aes(x = LX2, y = LY2)) +
    theme_minimal() +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) +
    geom_point(color = "mediumorchid4")