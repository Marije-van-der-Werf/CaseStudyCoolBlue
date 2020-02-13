###################################################################################################
# Script voor "grote X" na Coolblue gesprek
###################################################################################################

library(dplyr)
library(lubridate)
library(robustHD)
library(glmnet)

# source("data_inlezen.R")
# source("dummies.R")
source("create_modeldata.R")

y <- modelData$sales
x <- modelData[, c(3:53)] # alle x'en
D <- modelData[, c(54:82)] # alle dummies

x <- x[, colSums(x != 0) > 0]

#' Cross effects
crossx <- matrix(NA, nrow(x), 357)
x <- cbind(x, crossx)
count <- 43
for (i in 1:26){
    for (j in (i+1):27){
        if (sum(abs(x[,i] * x[,j])) != 0){ # hier worden al de kolommen die gelijk aan nul zijn eruit gehaald
            x[,count] <- x[,i] * x[,j]
            names(x)[count] <- paste(names(x)[i], names(x)[j], sep = " ")
            count <- count + 1 
        }
    }
}

count <- 347
for (i in 1:26){
    if (sum(abs(x[,i] * x[,33])) != 0){
        x[,count] <- x[,i] * x[,33]
        names(x)[count] <- paste(names(x)[i], names(x)[33], sep = " ")
        count <- count + 1
    }
}

count <- 373
for (i in 1:26){
    if (sum(abs(x[,i] * x[,30])) != 0){
        x[,count] <- x[,i] * x[,30]
        names(x)[count] <- paste(names(x)[i], names(x)[30], sep = " ")
        count <- count + 1
    }
}

x[,399] <- x[,30] * x[,33]
names(x)[399] <- paste(names(x)[30], names(x)[33], sep = " ")
count <- count + 1

x <- data.matrix(x)
D <- data.matrix(D)
adX <- x[,c(1:27, 30:32, 36:42)] ##Hier alle x'en die in de adstock gaan # Kolommen 36:42 zijn voor de spillovereffecten, nog checken of deze in de adstock moeten, of misschien zelfs een eigen adstock moeten
noAdX <- x[, c(28, 29, 33:35, 43:399)] ## Hier alle x'en die niet in de adstock gaan

Test <- modelData %>% 
    select(date) %>% 
    arrange(date) %>% 
    mutate(n = 1:1100)
Trend <- data.matrix(Test)

##Adstock
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

##model
trend <- Trend/365
ols <- lm(log(y) ~ trend[,2] + st_ad + st_x + D)
summary(ols)

#Lasso
X <- cbind(trend[,2], st_ad, st_x, D)
# set.seed(1234)
# cv <- cv.glmnet(X, log(y), alpha = 1, standardize = FALSE, penalty.factor = rep(c(1,0), c(58, 27)))
# model <- glmnet(X, log(y), alpha = 1, lambda = ((cv$lambda.min+cv$lambda.1se)/2), standardize = FALSE, penalty.factor = rep(c(1,0), c(58, 27))) 
# coef(model)
