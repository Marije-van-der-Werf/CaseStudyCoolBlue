###################################################################################################
# Script for ECM with variables coming from lasso for Belgium
###################################################################################################

#' Data and libraries
library(car)
library(ecm)
library(dplyr)
library(matrixStats)
library(tibble)
source("overlapping_group_lasso_BE.R")

var_vec <- as.vector(res[["var"]][[18]])
X_pred <- X[ ,c(unique(var_vec))]

ols_model <- lm(log(y) ~ X_pred)
durbinWatsonTest(ols_model)
acf(residuals(ols_model))

ols_model <- lm(log(y) ~ X_pred + lag(log(y)) + lag(X_pred))
acf(residuals(ols_model))

X_pred <- as.data.frame(X_pred)
D <- as.data.frame(D)

xtr <- X_pred %>% 
  select(- V1)
xeq <- X_pred %>% 
  select(- c(contains("DR"), colnames(D)))
ECM_model <- ecm(log(y), xeq, xtr, includeIntercept=TRUE)
summary(ECM_model)

#' Remove variables which will not be used anymore
rm(ols_model, X_pred, xeq, xtr, var_vec)
