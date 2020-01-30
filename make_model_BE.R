library(dplyr)
library(lubridate)
library(ggplot2)
library(robustHD)
library(caret)
library(glmnet)

source("data_inlezen.R")
source("dummies.R")
source("create_modeldata_table_BE.R")

y <- modelDataBE$sales
constant <- rep(1, 1100)
x <- modelDataBE[,c(3:34)] ## alle x'en
D <- modelDataBE[, c(35:59)] ## alle dummies
D <- cbind(constant, D)

x <- x %>%
    mutate(media_visibility = media * visibility,
           media_desktop = media * desktop,
           media_mediaS = media * mediaS,
           media_retargetingS = media * retargetingS,
           media_desktopS = media * desktopS,
           media_mobileDesktopS = media * mobileDesktopS,
           media_DR = media * DR,
           visibility_desktop = visibility * desktop,
           visibility_mediaS = visibility * mediaS,
           visibility_retargetingS = visibility * retargetingS,
           desktop_mediaS = desktop * mediaS,
           desktop_retargetingS = desktop * retargetingS,
           desktop_desktopS = desktop * desktopS,
           desktop_mobileDesktopS = desktop * mobileDesktopS,
           desktop_DR = desktop * DR,
           mediaS_retargetingS = mediaS * retargetingS,
           mediaS_desktopS = mediaS * desktopS,
           mediaS_mobiledesktop = mediaS * mobileDesktopS,
           mediaS_DR = mediaS * DR,
           retargetingS_desktopS = retargetingS * desktopS,
           retargetingS_mobiledesktopS = retargetingS * mobileDesktopS,
           desktopS_mobiledesktopS = desktopS * mobileDesktopS,
           desktopS_DR = desktopS * DR,
           mobileDesktopS_DR = mobileDesktopS * DR
    )






x <- data.matrix(x)
D <- data.matrix(D)
adX <- x[,c(1:24, 27:29)] ##Hier alle x'en die in de adstock gaan
noAdX <- x[, c(25, 26, 30:56)] ## Hier alle x'en die niet in de adstock gaan

#Frisch Waugh

#eta = residuals(lm(y ~ D))
#u = residuals(lm(x ~ D))
# ols
#coef(lm(y ~ -1 + D + x + Trend[,2]))
# fwl ols
#coef(lm(eta ~ -1 + u))

Test <- modelDataBE %>% 
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
ols <- lm(log(y) ~ -1 + trend[,2] + D + st_ad + st_x)
summary(ols)

#Plot residuals hele model
resi <- residuals(ols)
resi <- data.matrix(resi)
t <- ggplot(modelDataBE, aes(x = date))
t <- t + geom_line(aes(y = resi, colour = "Residuals"))
t

#Lasso
X <- cbind(trend[,2], st_ad, st_x, D)
set.seed(1234)
cv <- cv.glmnet(X, log(y), alpha = 1, standardize = FALSE, penalty.factor = rep(c(1,0), c(34,25)))
model <- glmnet(X, log(y), alpha = 1, lambda = cv$lambda.1se, standardize = FALSE, penalty.factor = rep(c(1,0), c(57, 25))) 
coef(model)

#model trend + dummies
ols2 <- lm(log(y) ~ -1 + trend[,2] + D)
summary(ols2)
resi2 <- residuals(ols2)
t2 <- ggplot(modelDataBE, aes(x=date))
t2 <- t2 + geom_line(aes(y=resi2, colour = "Residuals"))
t2


#Model alleen trend (+ constante)
ols3 <- lm(log(y) ~ trend[,2])
summary(ols3)
resi3 <- residuals(ols3)
resi3 <- data.matrix(resi3)

t3 <- ggplot(modelDataBE, aes(x = date))
t3 <- t3 + geom_line(aes(y = resi3, colour = "Residuals"))
t3

#Model alleen dummies
ols4 <- lm(log(y) ~ D)
summary(ols4)
resi4 <- residuals(ols4)
resi4 <- data.matrix(resi4)

t4 <- ggplot(modelDataBE, aes(x = date))
t4 <- t4 + geom_line(aes(y = resi4, colour = "Residuals"))
t4
