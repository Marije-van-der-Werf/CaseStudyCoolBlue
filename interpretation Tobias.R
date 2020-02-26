###################################################################################################
# Script voor interpretatie van elasticiteit 1 10 100
###################################################################################################
source("ECM_model.R")

Variabelen <- Elasticity$vars

interpretation <- matrix(NA, nrow = length(Variabelen), ncol = 3)
colnames(interpretation) <- c("vars", "stijgingCost", "stijgingSales")

Alles <- modelData %>% 
  left_join(y = as.data.frame(x))

# 
# VariablesMean <- colMeans(Alles[,Variabelen])
# SalesMean <- mean(Alles[,"sales"])

AllMeans <- c()
Elasticity2 <- cbind(Variabelen, NA)
for(i in 1:length(Variabelen)){
  A <- Alles %>% 
    select(sales, Variabelen[i]) %>% 
    rename("This" = Variabelen[i]) %>% 
    filter(This > 0)
  
  gem <- colMeans(A) # Baseren nu de stijging in sales op basis van de gemiddelde kosten (kosten moeten groter dan nul zijn) en de bijbehorende gemiddelde sales
  AllMeans <- cbind(AllMeans, c(Variabelen[i], gem[2]))
  if (gem[2] < 10){
    j <- 1
  }
  else if (gem[2] >= 10 && gem[2] < 100){
    j <- 10
  }
  else if (gem[2] >= 100 && gem[2] < 500){
    j <- 100
  }
  else{
    j <- 1000
  }
  
  
  interpretation[i, 1] <- Variabelen[i]
  interpretation[i, 2] <- j
  percentage <- ((j+gem[2])/gem[2])-1
  interpretation[i, 3] <- Elasticity[i ,4] * percentage * gem[1] * (gem[2] / (1 + gem[2]))
  Elasticity2[i , 2] <- Elasticity[i ,4]  * (gem[2] / (1 + gem[2]))
}

summary(as.numeric(AllMeans[2,]))
