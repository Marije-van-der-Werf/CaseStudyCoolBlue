###################################################################################################
# Script for grouped lasso with our selfmade crosseffects, spillover and dummies
###################################################################################################

library(gglasso)
source("lassorutger.R")
source("make_model_spillover.R")

#' Make groups of variables
var <- c(1:39, 64:90)
group <- c(1:39, rep(40, 27))
group <- c(group, rep(41:64, each = 3))
var <- c(var, 40, 22, 25, 41, 22, 23, 42, 22, 15, 43, 22, 6, 44, 22, 32, 45, 22, 31, 46, 15, 25, 47, 15, 23, 48, 15, 6, 49, 15, 32, 50, 15, 31, 51, 6, 25, 52, 6, 23, 53, 6, 32, 54, 6, 9, 55, 25 , 32, 56, 25, 33, 57, 25, 9, 58, 32, 9, 59, 32, 23, 60, 9, 23, 61, 31, 6, 62, 31, 25, 63, 2, 13)

#' Weights
w <-  rep(1,64)
w[1] <- 0
w[40] <- 0
w[41:64] <- sqrt(3)

res <- LASSOLISANNE(X, log(y), var, group, weight = w, loss = "ls")
res[["beta"]][[39]] # result of the 39st lambda 