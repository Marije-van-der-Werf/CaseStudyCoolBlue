###################################################################################################
# Script for long term elasticities and interpretation for Belgium
###################################################################################################

#' Data and libraries
library(dplyr) 
source("compute_stdev_BE.R")

ForLongTerm <- indepVars %>% 
    select(- coeff)
InteractionEffects <- ForLongTerm %>%
    filter(str_detect(ForLongTerm$vars, " "))
SoloEffects <- anti_join(ForLongTerm, InteractionEffects, by = "vars")

Everything <- modelData %>%
    left_join(y = as.data.frame(x))

bigMatrix <- matrix(NA, nrow(SoloEffects), nrow(SoloEffects)) #' elasticities per variable and interaction effect
colnames(bigMatrix) <- SoloEffects$vars
rownames(bigMatrix) <- SoloEffects$vars

correctedElasticities <- matrix(NA, nrow(SoloEffects), nrow(SoloEffects)) #' elasticities corrected for the number of times they occur
colnames(correctedElasticities) <- SoloEffects$vars
rownames(correctedElasticities) <- SoloEffects$vars

for (i in 1:nrow(SoloEffects)){
    thisVar <- SoloEffects[i,1]
    interactionTerms <- InteractionEffects %>%
        filter(str_detect(InteractionEffects$vars, thisVar))
    colthis <- Everything %>%
        select(thisVar)
    positivethis <- colthis %>%  
        filter(colthis > 0)
    
    AllWeNeed <- list()
    AllWeNeed[["gammai"]] <- SoloEffects[SoloEffects$vars == thisVar, 2] 
    AllWeNeed[["x1"]] <- c(AllWeNeed[["x1"]], colMeans(positivethis))
    AllWeNeed[["sigmai"]] <- SoloEffects[SoloEffects$vars == thisVar, 3]
    
    diagEffect <- (1/AllWeNeed[["sigmai"]])*AllWeNeed[["gammai"]] * (AllWeNeed[["x1"]]/(1+AllWeNeed[["x1"]]))
    bigMatrix[i,i] <- diagEffect
    correctedElasticities[i,i] <- diagEffect
    
    if(nrow(interactionTerms) == 0){
        bigMatrix[i,-i] <- NA
        bigMatrix[-i,i] <- NA
    } else{
        IAVars <- str_remove(interactionTerms[,1], thisVar)
        IAVars <- str_remove(IAVars, " ")
        
        rownames(interactionTerms) <- IAVars
        
        for(j in IAVars){
            gammasim <- interactionTerms[, c(1,2)]
            AllWeNeed[["gammasim"]] <- gammasim[j,2]
            
            sigmam <- SoloEffects[SoloEffects$vars == j, c(1,3)][2]  
            mu2 <- mean(log(1+Everything[,j]))
            tmp <- Everything %>% 
                select(sales, j) %>% 
                rename("tmp2" = j) %>% 
                filter(tmp2 > 0)
            meanX2 <- mean(tmp$tmp2)
            columnIndex <- which(names(bigMatrix) == j)
            columnIndex <- grep(j, colnames(bigMatrix))
            
            col <- Everything %>%
                select(j) 
            positive <- col %>%
                filter(col > 0)
            AllWeNeed[["correction"]] <- c(AllWeNeed[["correction"]], nrow(positive)/1100)
            
            bigMatrix[i, j] <- as.double((1/AllWeNeed[["sigmai"]])*(AllWeNeed[["x1"]]/(1+AllWeNeed[["x1"]]))*AllWeNeed[["gammasim"]]*((log(1+meanX2)-mu2)/sigmam))
            correctedElasticities[i,j] <- as.double((1/AllWeNeed[["sigmai"]])*(AllWeNeed[["x1"]]/(1+AllWeNeed[["x1"]]))*AllWeNeed[["correction"]]*AllWeNeed[["gammasim"]]*((log(1+meanX2)-mu2)/sigmam))
        }
    }
}

for(i in 1:nrow(correctedElasticities)){
    for(j in 1:ncol(correctedElasticities)){
        if(is.na(correctedElasticities[i,j])){
            correctedElasticities[i,j] <- 0
        }
    }
}

TotalEffect = rowSums(correctedElasticities)

variables <- names(correctedElasticities[,1])
interpretation <- matrix(NA, nrow = length(variables), ncol = 3)
colnames(interpretation) <- c("vars", "stijgingCost", "stijgingSales")

for(i in 1:length(variables)){
    tmp <- Everything %>% 
        select(sales, variables[i]) %>% 
        rename("This" = variables[i]) %>% 
        filter(This > 0)
    
    gem <- colMeans(tmp) 
    if(0.1*gem[2] < 1){
        j <- round(0.1*gem[2], digits = 2)
    } else{
        j <- round(0.1*gem[2])
    }
    
    interpretation[i, 1] <- variables[i]
    interpretation[i, 2] <- j
    interpretation[i, 3] <- round(as.double(TotalEffect[i])* 0.1 * as.double(gem[1]), digits = 2)
}

#' Remove variables which will not be used anymore
rm(list=setdiff(ls(), c("bigMatrix", "interpretation", "correctedElasticities")))