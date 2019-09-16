## make heatmap & eigen/SVD break downs

library(pheatmap)
# load(file=paste0("/Data/ADNI/Examples/amerge_subset.rda"))
# 
# 
# # select the continuous only subset
# continuous_data_subset <- amerge_subset[,which(variable_type_map[,"Continuous"]==1)]


vis_pca_data <- wine$objective
rownames(vis_pca_data) <- NULL
colnames(vis_pca_data) <- NULL
scaled_vis_pca_data <- scale(vis_pca_data)
cor_mat <- cor(scaled_vis_pca_data)

## output this as a rectangle of a certain size

png("./images/Eigen_SVD/data.png", height = 700, width = 200, pointsize = 20)
pheatmap(scaled_vis_pca_data, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F)
dev.off()

png("./images/Eigen_SVD/cor_mata.png", height = 200, width = 200, pointsize = 20)
pheatmap(cor_mat, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F)
dev.off()


svd_res <- svd(scaled_vis_pca_data)

png("./images/Eigen_SVD/U.png", height = 700, width = 200, pointsize = 20)
pheatmap(svd_res$u, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F)
dev.off()

png("./images/Eigen_SVD/D.png", height = 200, width = 200, pointsize = 20)
pheatmap(diag(svd_res$d), cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F)
dev.off()

png("./images/Eigen_SVD/L.png", height = 200, width = 200, pointsize = 20)
pheatmap(diag(svd_res$d^2), cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F)
dev.off()

png("./images/Eigen_SVD/Vt.png", height = 200, width = 200, pointsize = 20)
pheatmap(t(svd_res$v), cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F)
dev.off()


png("./images/Eigen_SVD/V.png", height = 200, width = 200, pointsize = 20)
pheatmap(svd_res$v, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F)
dev.off()
