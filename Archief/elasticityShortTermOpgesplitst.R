###################################################################################################
# Script voor elasticiteit inclusief interaction-effect (opgesplitst)
# Voor direct effect alleen
###################################################################################################

#' Data and libraries
library(dplyr) 
source("compute_stdev.R")

InteractionEffects <- ForShortTerm %>%
    filter(str_detect(ForShortTerm$vars, " "))
SoloEffects <-  anti_join(ForShortTerm, InteractionEffects, by = "vars")

Everything <- modelData %>% 
    left_join(y = as.data.frame(x))

bigMatrix <- matrix(NA, nrow = nrow(SoloEffects), ncol = nrow(SoloEffects)) #' this is the end result
rownames(bigMatrix) <- SoloEffects[,1]
colnames(bigMatrix) <- SoloEffects[,1]

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
    
    if(nrow(interactionTerms) != 0){
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
        }
    }
}
