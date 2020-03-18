###################################################################################################
# Script for short term elasticities and interpretation for Belgium
###################################################################################################

#' Data and libraries
library(dplyr) 
source("compute_stdev_BE.R")

InteractionEffects <- ForShortTerm %>%
    filter(str_detect(ForShortTerm$vars, " "))
SoloEffects <-  anti_join(ForShortTerm, InteractionEffects, by = "vars")

Everything <- modelData %>% 
    left_join(y = as.data.frame(x))

bigMatrix <- matrix(NA, nrow = nrow(SoloEffects), ncol = nrow(SoloEffects)) #' elasticities per variable and interaction effect
rownames(bigMatrix) <- SoloEffects[,1]
colnames(bigMatrix) <- SoloEffects[,1]

Elasticity <- matrix(NA, nrow(SoloEffects), 2) #' elasticities per variable, no interaction effects

for(i in 1:nrow(SoloEffects)){
    thisVar <- SoloEffects[i,1]
    interactionTerms <- InteractionEffects %>%
        filter(str_detect(InteractionEffects$vars, thisVar))
    AllWeNeed <- list()
    AllWeNeed[["alpha"]] <- SoloEffects[SoloEffects$vars == thisVar, 2]
    colthis <- Everything %>%
        select(thisVar) 
    positivethis <- colthis %>%
        filter(colthis > 0)
    
    AllWeNeed[["x1"]] <- c(AllWeNeed[["x1"]], colMeans(positivethis))
    AllWeNeed[["sigmaThis"]] <- SoloEffects[SoloEffects$vars == thisVar, 3]
    
    bigMatrix[i,i] <- (1/AllWeNeed[["sigmaThis"]]) * AllWeNeed[["alpha"]] * (AllWeNeed[["x1"]]/(1+AllWeNeed[["x1"]]))
    
    if(nrow(interactionTerms) == 0){
        ElasticityTmp <- (1/AllWeNeed[["sigmaThis"]])*AllWeNeed[["alpha"]] * (AllWeNeed[["x1"]]/(1+AllWeNeed[["x1"]]))
        Elasticity[i,1] <- thisVar
        Elasticity[i,2] <- ElasticityTmp
    }  
    else{
        IAVars <- str_remove(interactionTerms[,1], thisVar) 
        IAVars <- str_remove(IAVars, " ") 
        interactionTermsIAVars <- cbind(interactionTerms[, 1:2], IAVars)
        
        for(j in IAVars){ 
            beta <- interactionTermsIAVars[interactionTermsIAVars$IAVars == j, c(2,3)]
            AllWeNeed[["beta"]] <- beta[,1]
            stdevIAVars <- SoloEffects[SoloEffects$vars == j, c(1,3)]
            AllWeNeed[["sigmaIA"]] <- stdevIAVars[,2]
            mu2 <- mean(log(1+Everything[,j]))
            AllWeNeed[["muIA"]] <- mu2
            tmp <- Everything %>% 
                select(sales, j) %>% 
                rename("tmp2" = j) %>% 
                filter(tmp2 > 0)
            meanX2 <- mean(tmp$tmp2)
            AllWeNeed[["x2"]] <- meanX2
            
            bigMatrix[i,j] <- (1 / AllWeNeed[["sigmaThis"]]) * AllWeNeed[["beta"]] * ((log(1+AllWeNeed[["x2"]]) - AllWeNeed[["muIA"]]) / AllWeNeed[["sigmaIA"]]) * (AllWeNeed[["x1"]] / (1+AllWeNeed[["x1"]]))
            
            AllWeNeed[["beta"]] <- NULL
            AllWeNeed[["sigmaIA"]] <- NULL
            AllWeNeed[["muIA"]] <- NULL
            AllWeNeed[["x2"]] <- NULL
        }
        
        betas <- interactionTerms[, 1:2]
        AllWeNeed[["beta"]] <- betas[,2]
        
        for(j in IAVars){ 
            stdevIAVars <- SoloEffects[SoloEffects$vars == j, c(1,3)]
            AllWeNeed[["sigmaIA"]] <- c(AllWeNeed[["sigmaIA"]],stdevIAVars[,2])
            mu2 <- mean(log(1+Everything[,j]))
            AllWeNeed[["muIA"]] <- c(AllWeNeed[["muIA"]], mu2)
            A <- Everything %>% 
                select(sales, j) %>% 
                rename("Testjee" = j) %>% 
                filter(Testjee > 0)
            meanX2 <- mean(A$Testjee)
            AllWeNeed[["x2"]] <- c(AllWeNeed[["x2"]], meanX2)
        }
        
        for(k in interactionTerms[,1]){
            col <- Everything %>%
                select(k) 
            positive <- col %>%
                filter(col > 0)
            AllWeNeed[["correction"]] <- c(AllWeNeed[["correction"]], nrow(positive)/1100)
        }
        
        ElasticityTmp <- (1/AllWeNeed[["sigmaThis"]])*(AllWeNeed[["alpha"]] + t(AllWeNeed[["beta"]]* AllWeNeed[["correction"]]) %*% ((log(1+AllWeNeed[["x2"]])-AllWeNeed[["muIA"]])/AllWeNeed[["sigmaIA"]])) * (AllWeNeed[["x1"]]/(1+AllWeNeed[["x1"]]))
        Elasticity[i,1] <- thisVar
        Elasticity[i,2] <- ElasticityTmp
    }
}

Variables <- Elasticity[,1]

interpretation <- matrix(NA, nrow = length(Variables), ncol = 3) #' interpretation with increase in cost and corresponding increase in sales for each variable
colnames(interpretation) <- c("vars", "increaseCost", "increaseSales")

for(i in 1:length(Variables)){
    Tmp <- Everything %>% 
        select(sales, Variables[i]) %>% 
        rename("This" = Variables[i]) %>% 
        filter(This > 0)
    
    mean1 <- colMeans(Tmp)
    if(0.1 * mean1[2] < 1){
        j <- round(0.1 * mean1[2], digits = 2)
    } else{
        j <- round(0.1 * mean1[2])
    }
    
    interpretation[i, 1] <- Variables[i]
    interpretation[i, 2] <- j
    interpretation[i, 3] <- round(as.double(Elasticity[i ,2]) * 0.1 * as.double(mean1[1]), digits = 2)
}

#' Remove variables which will not be used anymore
rm(list=setdiff(ls(), c("bigMatrix", "interpretation", "Elasticity")))
