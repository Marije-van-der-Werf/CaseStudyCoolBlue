###################################################################################################
# Script for grouped lasso na Coolblue gesprek
###################################################################################################

library(gglasso)
library(stringr)
source("lassorutger.R")
source("model.R")

#' Make groups of variables
var <- c(1:43, 403:431) #' losse variabelen + dummies
group <- c(1:43, rep(44, 29))
for(i in 44:402){
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

res <- LASSOLISANNE(X, log(y), var, group, weight = w, loss = "ls")
res[["beta"]][[81]] # result of the 81 lambda, beetje random gekozen nu nog, moet nog een betere voor in de plaats komen