###################################################################################################
# Script voor interpretatie van elasticiteit (anders dan met de mean)
###################################################################################################
source("ECM_model.R")

Variabelen <- Elasticity$vars

Alles <- modelData %>% 
    left_join(y = as.data.frame(x))

B <- Alles %>% 
    select(date, sales)

for(i in 1:length(Variabelen)){
    A <- Alles %>% 
        select(date, sales, Variabelen[i]) %>% 
        rename("Testjee" = Variabelen[i]) %>% 
        filter(Testjee > 0) %>% 
        mutate(stijgingCost = round(0.01 * Testjee, 2),
               stijgingSales = round(Elasticity[i, 4] * sales * (Testjee / (1 + Testjee)), 2),
               winst = stijgingSales - stijgingCost)
    
    names(A)[names(A) == "Testjee"] <- Variabelen[i]
    names(A)[names(A) == "stijgingCost"] <- paste0("stijgingCost", Variabelen[i])
    names(A)[names(A) == "stijgingSales"] <- paste0("stijgingSales", Variabelen[i])
    names(A)[names(A) == "winst"] <- paste0("winst", Variabelen[i])
    
    B <- B %>% 
        left_join(y = A, by = c("date", "sales"))
}
