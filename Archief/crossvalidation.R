###################################################################################################
# Script for grouped lasso with our selfmade crosseffects, spillover and dummies
###################################################################################################

library(gglasso)
source("groupedLasso.R")
# library(mlmatrix)

res[["beta"]][[81]]

library(Metrics)
gem_rmse <- c()
lambdas <- res$lambda
for (i in 1:length(lambdas)) {
    
    # Extract coefficients and residula from each model
    b0_s <- as.vector(res[["b0"]][[i]])
    betas <- as.vector(as.matrix( res[["beta"]][[i]]))
    var_vec <- as.vector(res[["var"]][[i]])
    X_pred <- X[ ,c(var_vec)]
    
    rmse <- c()
    set.seed(14)
    for (k in 1:100){
        ind <- sample(1:nrow(X), nrow(X)*(9/10))
        X_train <- X_pred[ind,]
        y_train <- y[ind]
        X_test <- X_pred[-ind,]
        y_test <- y[-ind]
        
        y_testpred <- b0_s + betas %*% t(X_train)
        rmse[k] <- rmse(log(y_train), y_testpred)
    }
    
    gem_rmse[i] <- mean(rmse)
}
