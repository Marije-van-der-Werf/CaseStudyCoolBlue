library(dplyr)
library(lubridate)
library(ggplot2)
library(robustHD)
library(caret)
library(glmnet)

source("data_inlezen.R")
source("dummies.R")
source("create_modeldata_table.R")


y <- modelData$sales
x <- modelData[,c(3:34)] ## alle x'en
cleanx <- x
D <- modelData[, c(35:61)] ## alle dummies

crossx <- matrix(NA, nrow(x), 482)
x <- cbind(x,crossx)
count <- 33
for (i in 1:31){
    for (j in (i+1):32){
        if (sum(abs(x[,i] * x[,j])) != 0){
            x[,count] <- x[,i] * x[,j]
            names(x)[count] <- paste(names(x)[i], names(x)[j], sep = " ")
            count <- count + 1 
        }
    }
}


x <- data.matrix(x)
D <- data.matrix(D)
adX <- x[,c(1:24, 27:29)] ##Hier alle x'en die in de adstock gaan
noAdX <- x[, c(25, 26, 30:ncol(x))] ## Hier alle x'en die niet in de adstock gaan

#Frisch Waugh

#eta = residuals(lm(y ~ D))
#u = residuals(lm(x ~ D))
# ols
#coef(lm(y ~ -1 + D + x + Trend[,2]))
# fwl ols
#coef(lm(eta ~ -1 + u))

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
    for(i in 2:dimX[1])
    {
        adstock[i,j] = log(1+adX[i,j]) + lambda * adstock[i-1,j]
    }
}

adstock <- data.matrix(adstock)
colnames(adstock) <- colnames(adX)

st_x <- standardize(log(1+noAdX), centerFun = mean, scaleFun = sd)
st_ad <- standardize(adstock, centerFun = mean, scaleFun = sd)

##model
trend <- Trend/365
ols <- lm(log(y) ~ trend[,2] + D + st_ad + st_x)
summary(ols)

#Plot residuals hele model
resi <- residuals(ols)
resi <- data.matrix(resi)
t <- ggplot(modelData, aes(x = date))
t <- t + geom_line(aes(y = resi, colour = "Residuals"))
t

#Lasso
X <- cbind(trend[,2], st_ad, st_x, D)
set.seed(1234)
cv <- cv.glmnet(X, log(y), alpha = 1, standardize = FALSE, penalty.factor = rep(c(1,0), c(58, 
                                                                                          27)))
model <- glmnet(X, log(y), alpha = 1, lambda = ((cv$lambda.min+cv$lambda.1se)/2), standardize = 
                    FALSE, penalty.factor = rep(c(1,0), c(58, 27))) 
coef(model)

#model trend + dummies
ols2 <- lm(log(y) ~ -1 + trend[,2] + D)
summary(ols2)
resi2 <- residuals(ols2)
t2 <- ggplot(modelData, aes(x=date))
t2 <- t2 + geom_line(aes(y=resi2, colour = "Residuals"))
t2


#Model alleen trend (+ constante)
ols3 <- lm(log(y) ~ trend[,2])
summary(ols3)
resi3 <- residuals(ols3)
resi3 <- data.matrix(resi3)

t3 <- ggplot(modelData, aes(x = date))
t3 <- t3 + geom_line(aes(y = resi3, colour = "Residuals"))
t3

#Model alleen dummies
ols4 <- lm(log(y) ~ D)
summary(ols4)
resi4 <- residuals(ols4)
resi4 <- data.matrix(resi4)

t4 <- ggplot(modelData, aes(x = date))
t4 <- t4 + geom_line(aes(y = resi4, colour = "Residuals"))
t4
