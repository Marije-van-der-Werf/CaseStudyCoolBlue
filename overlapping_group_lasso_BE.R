###################################################################################################
# Script for overlapping group lasso for Belgium
###################################################################################################

#' Data and libraries
library(gglasso)
library(stringr)
source("interaction_effects_BE.R")
source("lasso_corrected.R")

#' Make groups of variables
var <- c(1:41, 352:378) #' variables on their own + dummies
group <- c(1:41, rep(42, 27))
for(i in 42:351){ #'  variables in interaction effects
    first = str_split(colnames(X)[i], " ", simplify = TRUE)[1]
    second= str_split(colnames(X)[i], " ", simplify = TRUE)[2]
    var = c(var, i, which(colnames(X) == first), which(colnames(X) == second))
}
group <- c(group, rep(43:352, each = 3))

#' Weights
w <-  rep(1, 352)
w[1] <- 0
w[42] <- 0
w[43:352] <- sqrt(3)

res <- LASSOCORRECTED(X, log(y), var, group, weight = w, loss = "ls")
res[["beta"]][[18]] # result of the 18th lambda

#' Remove variables which will not be used anymore
rm(first, group, i, second, var, w)
