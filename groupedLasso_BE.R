###################################################################################################
# Script for grouped lasso na Coolblue gesprek
###################################################################################################

library(gglasso)
library(stringr)
source("lassorutger.R")
source("model_BE.R")

#' Make groups of variables
var <- c(1:41, 352:378) #' losse variabelen + dummies
group <- c(1:41, rep(42, 27))
for(i in 42:351){
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

res <- LASSOLISANNE(X, log(y), var, group, weight = w, loss = "ls")
res[["beta"]][[18]] # result of the 18th lambda, beetje random gekozen nu nog, moet nog een betere voor in de plaats komen