###################################################################################################
# Script voor interpretatie van elasticiteit 
###################################################################################################
source("ECM_model.R")

Variabelen <- Elasticity$vars

interpretation <- matrix(NA, nrow = length(Variabelen), ncol = 3)
colnames(interpretation) <- c("vars", "stijgingCost", "stijgingSales")

Alles <- modelData %>% 
    left_join(y = as.data.frame(x))

for(i in 1:length(Variabelen)){
    A <- Alles %>% 
        select(sales, Variabelen[i]) %>% 
        rename("Testjee" = Variabelen[i]) %>% 
        filter(Testjee > 0)
    
    gem <- colMeans(A) # Baseren nu de stijging in sales op basis van de gemiddelde kosten (kosten moeten groter dan nul zijn) en de bijbehorende gemiddelde sales
    
    interpretation[i, 1] <- Variabelen[i]
    interpretation[i, 2] <- 0.01 * gem[2]
    interpretation[i, 3] <- Elasticity[i, 4] * gem[1] * (gem[2] / (1 + gem[2]))
}
