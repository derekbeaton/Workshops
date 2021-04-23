### usefully minimal versions of PLSC and PLSR.


## PLSC
baby_plsc <- function(X, Y, center_X = T, center_Y = T, scale_X = F, scale_Y = F){
  
  X <- scale(X, center = center_X, scale = scale_X)
  Y <- scale(Y, center = center_Y, scale = scale_Y)
  
  R <- t(X) %*% Y
  R_svd <- svd(R)
  
  R_svd$LX <- X %*% R_svd$u
  R_svd$LY <- Y %*% R_svd$v
  
  R_svd
}


## PLSR
baby_plsr <- function(X, Y, center_X = T, center_Y = T, scale_X = F, scale_Y = F){
  
  X_original <- X <- scale(X, center = center_X, scale = scale_X)
  Y_original <- Y <- scale(Y, center = center_Y, scale = scale_Y)
 
  Y_center <- attributes(Y)$`scaled:center`
  Y_scale <- attributes(Y)$`scaled:scale`
  if(is.null(Y_center)){
    Y_center <- rep(0,ncol(Y_original))
  }
  if(is.null(Y_scale)){
    Y_scale <- rep(1,ncol(Y_original))
  }
  
  X_svd <- svd(X)
  max_rank <- length(X_svd)
  X_trace <- sum(X_svd$d^2)
  
  Y_svd <- svd(Y)
  Y_trace <- sum(Y_svd$d^2)
  
  
  
  LX <- tx <- matrix(0,nrow(X),max_rank)
  LY <- matrix(0,nrow(Y),max_rank)
  
  pred_u <- u <- matrix(0,ncol(X),max_rank)  
  v <- matrix(0,ncol(Y),max_rank)
  
  r2.x.cumulative <- r2.y.cumulative <- d <- betas <- vector("numeric",max_rank)
  
  X_reconstructed_array <- X_residual_array <- array(0,dim=c(nrow(X),ncol(X),max_rank))
  Y_reconstructed_array <- Y_residual_array <- array(0,dim=c(nrow(Y),ncol(Y),max_rank))
  
  
  
  for(i in 1:max_rank){
  
    ## PLSC:
    plsc_svd <- baby_plsc(X,Y,F,F,F,F)
    
    ## Store lots of things
    u[,i] <- plsc_svd$u[,1,drop=F]
    v[,i] <- plsc_svd$v[,1,drop=F]
    d[i] <- plsc_svd$d[1]
    LX[,i] <- plsc_svd$LX[,1,drop=F]
    LY[,i] <- plsc_svd$LY[,1,drop=F]
    
    
    ## Making some new stuff we need because of PLSR
    tx[,i] <- LX[,i] / sqrt(sum(LX[,i]^2))
    betas[i] <- t(LY[,i]) %*% tx[,i]
    pred_u[,i] <- t(tx[,i]) %*% X
  
    ## Build new (rank 1) versions of X and Y
    X_reconstructed_array[,,i] <- tx[,i] %o% pred_u[,i]
    Y_reconstructed_array[,,i] <- (tx[,i] * betas[i]) %o% v[,i]
    
    ## Subtract X and Y from the new rank 1 versions, these are now rank 1 residualized versions
    X_residual_array[,,i] <- X - X_reconstructed_array[,,i]
    Y_residual_array[,,i] <- Y - Y_reconstructed_array[,,i]
    
    ## Make X & Y the residuals and we repeat the loop
    X <- X_residual_array[,,i]
    Y <- Y_residual_array[,,i]
    
    ## Store R2 per component for X & Y
    r2.x.cumulative[i] <- (X_trace-sum(X^2)) / X_trace
    r2.y.cumulative[i] <- (Y_trace-sum(Y^2)) / Y_trace
  
  }
  

  Y_reconstructed <- (tx %*% diag(betas) %*% t(v))
  Y_residuals <- Y_original - Y_reconstructed
    ## here we are first multiplying by the original scaling factor (e.g., the sd), then adding the center back in
  Y_fitted_values <-  Y_reconstructed * matrix(Y_scale,nrow(Y_original),ncol(Y_original),byrow=T) + matrix(Y_center,nrow(Y_original),ncol(Y_original),byrow=T)
  
  
  list(u = u, v = v, LX = LX, LY = LY, 
       d = d, betas = betas, tx = tx, 
       r2.x.cumulative = r2.x.cumulative, r2.y.cumulative = r2.y.cumulative,
       X_reconstructed_array = X_reconstructed_array,
       X_residual_array = X_residual_array,
       Y_reconstructed_array = Y_reconstructed_array,
       Y_residual_array = Y_residual_array,
       Y_reconstructed = Y_reconstructed,
       Y_fitted_values = Y_fitted_values,
       Y_residuals = Y_residuals)
}