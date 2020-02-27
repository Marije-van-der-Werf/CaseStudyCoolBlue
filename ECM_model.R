###################################################################################################
# Script voor ECM met variabelen die uit lasso komen, plus de elasticiteiten berekenen
###################################################################################################

library(car)
library(ecm)
library(dplyr)
library(matrixStats)
library(tibble)
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

#' Elasticiteiten bepalen
Test1 <- t(colSds(adstock)) # bepaal standaard deviaties van variabelen in adstock
colnames(Test1) <- colnames(adstock)
Test1 <- as.data.frame(t(Test1)) %>% 
    rownames_to_column("vars") 

Test2 <- t(colSds(log(1+noAdX))) # bepaal standaard deviaties van variabelen niet in adstock
colnames(Test2) <- colnames(noAdX)
Test2 <- as.data.frame(t(Test2)) %>% 
    rownames_to_column("vars")

Test <- rbind(Test1, Test2) # zet alle standaard deviaties in 1 dataframe

coeff <- ECM_model1[["coefficients"]] # zet coefficienten uit ECM in dataframe
Test3 <- as.data.frame(coeff) %>% 
    rownames_to_column("vars")

Test3$vars <- gsub("`", "", Test3$vars) # zorg dat namen overeenkomen 
Test3$vars <- gsub("delta", "", Test3$vars)

Test3 <- Test3 %>% # zet de standaard deviaties naast de coefficienten
    left_join(y = Test, by = "vars") %>% 
    filter(!is.na(V1)) # hou alleen variabelen over die "op de delta manier" in ons model zitten, dus niet de lagged variables

Elasticity <- Test3 %>% 
    mutate(elasticiteit = coeff / V1) %>%  # bereken elasticiteit
    rename("StDev" = "V1")

###################################################################################################
coeff <- ECM_model1[["coefficients"]] # zet coefficienten uit ECM in dataframe
Test4 <- as.data.frame(coeff) %>% 
    rownames_to_column("vars")

Test4$vars <- gsub("`", "", Test4$vars) # zorg dat namen overeenkomen 
Test4$Match <- grepl(pattern = "Lag1$", Test4$vars) # selecteer de "Lag1 variabelen"
Test4 <- Test4 %>% 
    filter(Match == TRUE) %>% 
    select(- Match)
Test4$vars <- gsub("Lag1", "", Test4$vars) # zorg dat namen overeenkomen

yVars <- Test4 %>% 
    filter(vars == "y")
andereVars <- Test4 %>% 
    filter(vars != "y")

andereVars <- andereVars %>% 
    mutate(longTerm = andereVars$coeff / - yVars$coeff)

andereVars <- andereVars %>% 
    left_join(y = Test) %>% 
    rename("StDev" = "V1") %>% 
    mutate(elasticiteit = longTerm / StDev) %>% 
    filter(!is.na(elasticiteit))
