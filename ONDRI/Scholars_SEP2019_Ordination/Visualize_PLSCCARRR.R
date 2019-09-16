## pls cca & rrr

library(GSVD)


X <- scale(wine$objective)
Y <- scale(wine$subjective)
colnames(X) <- NULL
colnames(Y) <- NULL
rownames(X) <- NULL
rownames(Y) <- NULL

co_cov <- t(X) %*% Y
X_proj <- (crossprod(X) %^% (1/2))
Y_proj <- (crossprod(Y) %^% (1/2))



pheatmap::pheatmap(X, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F)
pheatmap::pheatmap(Y, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F)



pheatmap::pheatmap(X_proj, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F)
pheatmap::pheatmap(Y_proj, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F)


pheatmap::pheatmap(co_cov, cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(100), legend = F)
pheatmap::pheatmap( ((X_proj %^% (-1)) %^% (1/2)) %*% co_cov , cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(5), legend = F)
pheatmap::pheatmap((X_proj %^% (-1/2)) %*% co_cov %*% (Y_proj %^% (-1/2)) , cluster_rows = F, cluster_cols = F, color = viridisLite::cividis(1000), legend = F)

