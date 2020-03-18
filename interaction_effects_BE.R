###################################################################################################
# Script in which interaction effects are made and variables are standardized for Belgium
###################################################################################################

#' Data and libraries
library(dplyr)
library(lubridate)
library(robustHD)
library(glmnet)
source("create_modeldata_BE.R")

y <- modelData$sales
x <- modelData[, c(3:53)] # all variables
D <- modelData[, c(54:80)] # all dummies

x <- x[, colSums(x != 0) > 0]

#' Interaction effects
crossx <- matrix(NA, nrow(x), 310)
x <- cbind(x, crossx)
count <- 41
for (i in 1:24){
    for (j in (i+1):25){
        if (sum(abs(x[,i] * x[,j])) != 0){ #' remove all columns equal to zero
            x[,count] <- x[,i] * x[,j]
            names(x)[count] <- paste(names(x)[i], names(x)[j], sep = " ")
            count <- count + 1 
        }
    }
}

count <- 300
for (i in 1:25){
    if (sum(abs(x[,i] * x[,31])) != 0){
        x[,count] <- x[,i] * x[,31]
        names(x)[count] <- paste(names(x)[i], names(x)[31], sep = " ")
        count <- count + 1
    }
}

count <- 325
for (i in 1:25){
    if (sum(abs(x[,i] * x[,28])) != 0){
        x[,count] <- x[,i] * x[,28]
        names(x)[count] <- paste(names(x)[i], names(x)[28], sep = " ")
        count <- count + 1
    }
}

x[,350] <- x[,28] * x[,31]
names(x)[350] <- paste(names(x)[28], names(x)[31], sep = " ")
count <- count + 1

x <- data.matrix(x)
D <- data.matrix(D)
adX <- x[, c(1:25, 28:30, 34:40)] #' all variables which go in the adstock
noAdX <- x[, c(26, 27, 31:33, 41:350)] #' all variables which do not go in the adstock

tmp <- modelData %>% 
    select(date) %>% 
    arrange(date) %>% 
    mutate(n = 1:1100)
trend <- data.matrix(tmp) / 365

#' Make the adstock
dimX <- dim(adX)
adstock <- matrix(NA, nrow = dimX[1], ncol = dimX[2])
lambda <- 0.85

for(j in 1:(dimX[2])){
    adstock[1,j] = adX[1,j]
    for(i in 2:dimX[1]){
        adstock[i,j] = log(1+adX[i,j]) + lambda * adstock[i-1,j]
    }
}

adstock <- data.matrix(adstock)
colnames(adstock) <- colnames(adX)

st_x <- standardize(log(1+noAdX), centerFun = mean, scaleFun = sd)
st_ad <- standardize(adstock, centerFun = mean, scaleFun = sd)

#' All variables and dummies together
X <- cbind(trend[,2], st_ad, st_x, D)

#' Remove variables which will not be used anymore
rm(list=setdiff(ls(), c("adstock", "D", "noAdX", "X", "x", "y", "modelData")))
