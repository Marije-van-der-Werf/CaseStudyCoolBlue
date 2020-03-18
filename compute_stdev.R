###################################################################################################
# Script for computation of the standard deviations that are needed
###################################################################################################

#' Data and libraries
library(dplyr)
source("ECM_model.R")

#' For the short run
StDevAd <- t(colSds(adstock)) #' compute standard deviation for variables in adstock
colnames(StDevAd) <- colnames(adstock)
StDevAd <- as.data.frame(t(StDevAd)) %>% 
    rownames_to_column("vars") 

StDevNoAd <- t(colSds(log(1+noAdX))) #' compute standard deviation for variables not in adstock
colnames(StDevNoAd) <- colnames(noAdX)
StDevNoAd <- as.data.frame(t(StDevNoAd)) %>% 
    rownames_to_column("vars")

AllStDev <- rbind(StDevAd, StDevNoAd) #' put all standard deviations together

coeff <- ECM_model[["coefficients"]] #' take coefficients from ECM
ForShortTerm <- as.data.frame(coeff) %>% 
    rownames_to_column("vars")

ForShortTerm$vars <- gsub("`", "", ForShortTerm$vars) #' make sure the names are the same
ForShortTerm$vars <- gsub("delta", "", ForShortTerm$vars)

ForShortTerm <- ForShortTerm %>% #' put standard deviations next to the variables
    left_join(y = AllStDev, by = "vars") %>% 
    filter(!is.na(V1)) %>% #' only keep the shortterm variables
    rename("StDev" = "V1")

#' For the long run
ForLongTerm <- as.data.frame(coeff) %>% 
    rownames_to_column("vars")

ForLongTerm$vars <- gsub("`", "", ForLongTerm$vars) #' make sure the names are the same
ForLongTerm$Match <- grepl(pattern = "Lag1$", ForLongTerm$vars) #' select the long term variables
ForLongTerm <- ForLongTerm %>% 
    filter(Match == TRUE) %>% 
    select(- Match)
ForLongTerm$vars <- gsub("Lag1", "", ForLongTerm$vars) #' make sure the names are the same

depVar <- ForLongTerm %>% #' the dependent variable
    filter(vars == "y")
indepVars <- ForLongTerm %>% #' the independent variables
    filter(vars != "y")

indepVars <- indepVars %>% 
    mutate(longTerm = indepVars$coeff / - depVar$coeff)

indepVars <- indepVars %>% 
    left_join(y = AllStDev) %>% 
    rename("StDev" = "V1",
           "Gamma" = "longTerm") %>% 
    filter(!is.na(StDev))

#' Remove variables which will not be used anymore
rm(adstock, noAdX)
