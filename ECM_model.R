library(car)
library(ecm)
library(dplyr)
source("groupedLasso.R")


var_vec <- as.vector(res[["var"]][[81]])
X_pred <- X[ ,c(unique(var_vec))]

ols_model <- lm(log(y) ~ X_pred)
durbinWatsonTest(ols_model)
acf(residuals(ols_model))

ols_model <- lm(log(y) ~ X_pred + lag(log(y)) + lag(X_pred))
durbinWatsonTest(ols_model)
acf(residuals(ols_model))

X_pred <- as.data.frame(X_pred)
D <- as.data.frame(D)

xtr <- X_pred %>% 
    select(- V1)
xeq <- X_pred %>% 
    select(- c(contains("DR"), colnames(D)))
ECM_model1 <- ecm(log(y), xeq, xtr, includeIntercept=TRUE)
summary(ECM_model1)