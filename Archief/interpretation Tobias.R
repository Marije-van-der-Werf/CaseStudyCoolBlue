###################################################################################################
# Script voor interpretatie van elasticiteit 1 10 100
###################################################################################################
source("ECM_model.R")

Variabelen <- Elasticity$vars

interpretation <- matrix(NA, nrow = length(Variabelen), ncol = 3)
colnames(interpretation) <- c("vars", "stijgingCost", "stijgingSales")

Alles <- modelData %>% 
  left_join(y = as.data.frame(x))

AllMeans <- c()
Elasticity2 <- cbind(Variabelen, NA)
for(i in 1:length(Variabelen)){
  A <- Alles %>% 
    select(sales, Variabelen[i]) %>% 
    rename("This" = Variabelen[i]) %>% 
    filter(This > 0)
  
  gem <- colMeans(A) # Baseren nu de stijging in sales op basis van de gemiddelde kosten (kosten moeten groter dan nul zijn) en de bijbehorende gemiddelde sales
  AllMeans <- cbind(AllMeans, c(Variabelen[i], gem[2]))
  if (gem[2] < 2.5){
    j <- 1
  }
  else if (gem[2] >= 2.5 && gem[2] < 7.5){
    j <- 5
  }
  else if (gem[2] >= 7.5 && gem[2] < 25){
    j <- 10
  }
  else if (gem[2] >= 25 && gem[2] < 75){
    j <- 50
  }
  else if (gem[2] >= 75 && gem[2] < 250){
    j <- 100
  }
  else if (gem[2] >= 250 && gem[2] < 750){
    j <- 500
  }
  else if (gem[2] >= 750 && gem[2] < 2500){
    j <- 1000
  }
  else if (gem[2] >= 2500 && gem[2] < 7500){
    j <- 5000
  }
  else if (gem[2] >= 7500 && gem[2] < 25000){
    j <- 10000
  }
  else if (gem[2] >= 25000 && gem[2] < 75000){
    j <- 50000
  }
  else if (gem[2] >= 75000 && gem[2] < 250000){
    j <- 100000
  }
  else{
    j <- 500000
  }
  
  interpretation[i, 1] <- Variabelen[i]
  interpretation[i, 2] <- j
  percentage <- (((j+gem[2])/gem[2])-1)*100
  interpretation[i, 3] <- Elasticity[i ,4] * percentage * gem[1] * (gem[2] / (1 + gem[2]))
  Elasticity2[i , 2] <- Elasticity[i ,4]  * (gem[2] / (1 + gem[2]))
}

summary(as.numeric(AllMeans[2,]))
AllMeans[2,] <- as.numeric(AllMeans[2,])
AllMeans<-t(AllMeans)

###################################################################################################
VariabelenLong <- andereVars$vars

interpretationLong <- matrix(NA, nrow = length(VariabelenLong), ncol = 3)
colnames(interpretationLong) <- c("vars", "stijgingCost", "stijgingSales")

Alles <- modelData %>% 
    left_join(y = as.data.frame(x))
 
AllMeansLong <- c()
Elasticity2Long <- cbind(VariabelenLong, NA)
for(i in 1:length(VariabelenLong)){
    A <- Alles %>% 
        select(sales, VariabelenLong[i]) %>% 
        rename("This" = VariabelenLong[i]) %>% 
        filter(This > 0)
    
    gem <- colMeans(A) # Baseren nu de stijging in sales op basis van de gemiddelde kosten (kosten moeten groter dan nul zijn) en de bijbehorende gemiddelde sales
    AllMeansLong <- cbind(AllMeansLong, c(VariabelenLong[i], gem[2]))
    if (gem[2] < 2.5){
        j <- 1
    }
    else if (gem[2] >= 2.5 && gem[2] < 7.5){
        j <- 5
    }
    else if (gem[2] >= 7.5 && gem[2] < 25){
        j <- 10
    }
    else if (gem[2] >= 25 && gem[2] < 75){
        j <- 50
    }
    else if (gem[2] >= 75 && gem[2] < 250){
        j <- 100
    }
    else if (gem[2] >= 250 && gem[2] < 750){
        j <- 500
    }
    else if (gem[2] >= 750 && gem[2] < 2500){
        j <- 1000
    }
    else if (gem[2] >= 2500 && gem[2] < 7500){
        j <- 5000
    }
    else if (gem[2] >= 7500 && gem[2] < 25000){
        j <- 10000
    }
    else if (gem[2] >= 25000 && gem[2] < 75000){
        j <- 50000
    }
    else if (gem[2] >= 75000 && gem[2] < 250000){
        j <- 100000
    }
    else{
        j <- 500000
    }
    
    interpretationLong[i, 1] <- VariabelenLong[i]
    interpretationLong[i, 2] <- j
    percentage <- (((j+gem[2])/gem[2])-1)*100
    interpretationLong[i, 3] <- andereVars[i ,4] * percentage * gem[1] * (gem[2] / (1 + gem[2]))
    Elasticity2Long[i , 2] <- andereVars[i ,4]  * (gem[2] / (1 + gem[2]))
}

effecten <- as.data.frame(interpretation) %>% 
    full_join(y = as.data.frame(interpretationLong), by = c("vars", "stijgingCost")) %>% 
    rename("directEffect" = "stijgingSales.x", "longtermEffect" = "stijgingSales.y")

