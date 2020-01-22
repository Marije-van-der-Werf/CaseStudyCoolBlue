library(dplyr)
library(lubridate)

source("data_inlezen.R")
source("dummies.R")
source("create_modeldata_table.R")


y <- modelData$sales
constant <- rep(1,1100)
x <- modelData[,c(3:27)]
D <- modelData[, c(1,29:47)]
D <- cbind(constant, D)
D <- D %>% 
    left_join(FeestdagenNL, by = "date")




x <- data.matrix(x)
D <- data.matrix(D)


#Frisch Waugh

#eta = residuals(lm(y ~ D))
#u = residuals(lm(x ~ D))
# ols
#coef(lm(y ~ -1 + D + x + Trend[,2]))
# fwl ols
#coef(lm(eta ~ -1 + u))

Test <- modelData %>% 
    select(date) %>% 
    arrange(date) %>% 
    mutate(n = 1:1100)
Trend <- data.matrix(Test)

##Adstock
adstock <- matrix(, nrow = 1100, ncol = 25)
lambda <- 0.85
for(j in 1:25){
    
    
    adstock[1,j] = x[1,j]
    for(i in 2:1100)
    {
        adstock[i,j] = x[i,j] + lambda * adstock[i-1,j]
    }
}

adstock <- data.matrix(adstock)


##model
trend <- Trend/365
ols <- lm(log(y) ~ -1 + trend[,2] + D[,-c(2)] + log(1+adstock))
resi <- residuals(lm(log(y) ~ -1 + Trend[,2] + D + log(1+adstock)))
resi <- data.matrix(resi)

t <- ggplot(modelData, aes(x = date))
t <- t + geom_line(aes(y = resi, colour = "Residuals"))
t

