###################################################################################################
# Script for overlapping group lasso
###################################################################################################

#' Data and libraries
library(gglasso)
library(stringr)
source("interaction_effects.R")
source("lasso_corrected.R")

#' Make groups of variables
var <- c(1:43, 403:431) #' variables on their own + dummies
group <- c(1:43, rep(44, 29))
for(i in 44:402){ #' variables in interaction effects
    first = str_split(colnames(X)[i], " ", simplify = TRUE)[1]
    second= str_split(colnames(X)[i], " ", simplify = TRUE)[2]
    var = c(var, i, which(colnames(X) == first), which(colnames(X) == second))
}
group <- c(group, rep(45:403, each = 3))

#' Weights
w <-  rep(1, 403)
w[1] <- 0
w[44] <- 0
w[45:403] <- sqrt(3)

res <- LASSOCORRECTED(X, log(y), var, group, weight = w, loss = "ls")
res[["beta"]][[81]] # result of the 81th lambda

#' Remove variables which will not be used anymore
rm(first, group, i, second, var, w)
