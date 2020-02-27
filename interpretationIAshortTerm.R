###################################################################################################
# Script voor interpretatie van elasticiteit inclusief interaction-effect
# Voor direct effect alleen
# en tot nu toe alleen nog de elasticiteit, nogn iet de interpretatie
###################################################################################################
source("ECM_model.R")

VarCoef <- Elasticity[,1:3]
InteractionEffects <- VarCoef %>%
    filter(str_detect(VarCoef$vars, " "))
SoloEffects <-  anti_join(VarCoef, InteractionEffects, by = "vars")

Alles <- modelData %>% 
    left_join(y = as.data.frame(x))

ElasticityNew <- matrix(NA, nrow(SoloEffects), 2)

for (i in 1:nrow(SoloEffects)){
    thisVar <- SoloEffects[i,1]
    interactionTerms <- InteractionEffects %>%
        filter(str_detect(InteractionEffects$vars, thisVar))
    AllWeNeed <- list()
    AllWeNeed[["alpha"]] <- SoloEffects[SoloEffects$vars == thisVar, 2]
    colthis <- Alles %>%
        select(thisVar) 
    positivethis <- colthis %>%
        filter(colthis > 0)
    
    AllWeNeed[["x1"]] <- c(AllWeNeed[["x1"]], colMeans(positivethis))
    AllWeNeed[["sigmaThis"]] <- SoloEffects[SoloEffects$vars == thisVar, 3]
    
    if(nrow(interactionTerms) == 0){
        ElasticityN <- (1/AllWeNeed[["sigmaThis"]])*AllWeNeed[["alpha"]] * (AllWeNeed[["x1"]]/(1+AllWeNeed[["x1"]]))
        ElasticityNew[i,1] <- thisVar
        ElasticityNew[i,2] <- ElasticityN
    }  
    else{
        IAVars <- str_remove(interactionTerms[,1], thisVar) 
        IAVars <- str_remove(IAVars, " ") 
        
        betas <- interactionTerms[, 1:2]
        AllWeNeed[["beta"]] <- betas[,2]
        AllWeNeed[["sigmaThis"]] <- SoloEffects[SoloEffects$vars == thisVar, 3]
        
        
        for(j in IAVars){ 
            stdevIAVars <- SoloEffects[SoloEffects$vars == j, c(1,3)]
            AllWeNeed[["sigmaIA"]] <- c(AllWeNeed[["sigmaIA"]],stdevIAVars[,2])
            mu2 <- mean(log(1+Alles[,j]))
            AllWeNeed[["muIA"]] <- c(AllWeNeed[["muIA"]], mu2)
            A <- Alles %>% 
                select(sales, j) %>% 
                rename("Testjee" = j) %>% 
                filter(Testjee > 0)
            meanX2 <- mean(A$Testjee)
            AllWeNeed[["x2"]] <- c(AllWeNeed[["x2"]], meanX2)
        }
        
        for(k in interactionTerms[,1]){
            col <- Alles %>%
                select(k) 
            positive <- col %>%
                filter(col > 0)
            AllWeNeed[["correction"]] <- c(AllWeNeed[["correction"]], nrow(positive)/1100)
        }
        
        
        ElasticityN <- (1/AllWeNeed[["sigmaThis"]])*(AllWeNeed[["alpha"]] + t(AllWeNeed[["beta"]]* AllWeNeed[["correction"]]) %*% ((log(1+AllWeNeed[["x2"]])-AllWeNeed[["muIA"]])/AllWeNeed[["sigmaIA"]])) * (AllWeNeed[["x1"]]/(1+AllWeNeed[["x1"]]))
        ElasticityNew[i,1] <- thisVar
        ElasticityNew[i,2] <- ElasticityN
    }
}
