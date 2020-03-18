library(MLGL)
library(gglasso)
library(stringr)
source("lassorutger.R")

var <- c(1:33, 516:542) #' losse variabelen + dummies
group <- c(1:33, rep(34, 27))
for(i in 34:515){
    first = str_split(colnames(X)[i], " ", simplify = TRUE)[1]
    second= str_split(colnames(X)[i], " ", simplify = TRUE)[2]
    var = c(var, i, which(colnames(X) == first), which(colnames(X) == second))
}
group <- c(group, rep(35:516,each = 3))


w <-  rep(1,516)
w[1] <- 0
w[34] <- 0
w[35:516] <- sqrt(3)

res <- LASSOLISANNE(X, log(y), var, group, weight = w, loss = "ls")
res$beta 
res[["beta"]][[41]] # result of the 41st lambda 