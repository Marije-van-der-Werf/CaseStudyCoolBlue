library(MLGL)
library(gglasso)
library(stringr)
source("lassorutger.R")
#' Voor X run make_model tot en met regel 100

var <- c(1:33, 58:84)
group <- c(1:33, rep(34, 27))
group <- c(group, rep(35:57, each = 3))
var <- c(var, 34, 22, 25, 35, 22, 23, 36, 22, 15, 37, 22, 6, 38, 22, 32, 39, 22, 31, 40, 15, 25, 41, 15, 23, 42, 15, 6, 43, 15, 32, 44, 15, 31, 45, 6, 25, 46, 6, 23, 47, 6, 32, 48, 6, 9, 49, 25 , 32, 50, 25, 33, 51, 25, 9, 52, 32, 9, 53, 32, 23, 54, 9, 23, 55, 31, 6, 56, 31, 25)

w <-  rep(1,57)
w[1] <- 0
w[34] <- 0
w[35:57] <- sqrt(3)

res <- LASSOLISANNE(X, log(y), var, group, weight = w, loss = "ls")
res$beta 
res[["beta"]][[41]] # result of the 41st lambda 
