###################################################################################################
# Script in which interaction effects are made and variables are standardized 
###################################################################################################

#' Data and libraries
library(dplyr)
library(lubridate)
library(robustHD)
library(glmnet)
source("create_modeldata.R")

y <- modelData$sales
x <- modelData[, c(3:53)] #' all variables
D <- modelData[, c(54:82)] #' all dummies

x <- x[, colSums(x != 0) > 0]

#' Interaction effects
crossx <- matrix(NA, nrow(x), 359)
x <- cbind(x, crossx)
count <- 43
for (i in 1:26){
    for (j in (i+1):27){
        if (sum(abs(x[,i] * x[,j])) != 0){ #' remove all columns equal to zero
            x[,count] <- x[,i] * x[,j]
            names(x)[count] <- paste(names(x)[i], names(x)[j], sep = " ")
            count <- count + 1 
        }
    }
}

count <- 347
for (i in 1:27){
    if (sum(abs(x[,i] * x[,33])) != 0){
        x[,count] <- x[,i] * x[,33]
        names(x)[count] <- paste(names(x)[i], names(x)[33], sep = " ")
        count <- count + 1
    }
}

count <- 374
for (i in 1:27){
    if (sum(abs(x[,i] * x[,30])) != 0){
        x[,count] <- x[,i] * x[,30]
        names(x)[count] <- paste(names(x)[i], names(x)[30], sep = " ")
        count <- count + 1
    }
}

x[,401] <- x[,30] * x[,33]
names(x)[401] <- paste(names(x)[30], names(x)[33], sep = " ")
count <- count + 1

x <- data.matrix(x)
D <- data.matrix(D)
adX <- x[,c(1:27, 30:32, 36:42)] #' all variables which go in the adstock
noAdX <- x[, c(28, 29, 33:35, 43:401)] #' all variables which do not go in the adstock

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
