SalesnoOutlier <- Sales2093[Sales2093$products_sold < 1000,]
SalesnoOutlierNL <- SalesnoOutlier[SalesnoOutlier$subsidiary_id == 1,]

SalesnoOutlierNL <- SalesnoOutlierNL %>% 
    mutate(gem_prijs = sales / products_sold)

Sales2017 <- SalesnoOutlierNL[SalesnoOutlierNL$date < "2017-12-31",]
AvgSales2017 <- mean(Sales2017$products_sold)
AvgPrice2017 <- mean(Sales2017$gem_prijs)

Sales2018 <- SalesnoOutlierNL[(SalesnoOutlierNL$date < "2018-12-31" & SalesnoOutlierNL$date > "2017-12-31"),]
AvgSales2018 <- mean(Sales2018$products_sold)
AvgPrice2018 <- mean(Sales2018$gem_prijs)

Sales2019 <- SalesnoOutlierNL[(SalesnoOutlierNL$date < "2019-12-31" & SalesnoOutlierNL$date > "2018-12-31"),]
AvgSales2019 <- mean(Sales2019$products_sold)
AvgPrice2019 <- mean(Sales2019$gem_prijs)

Sales2020 <- SalesnoOutlierNL[(SalesnoOutlierNL$date < "2020-12-31" & SalesnoOutlierNL$date > "2019-12-31"),]
AvgSales2020 <- mean(Sales2020$products_sold)
AvgPrice2020 <- mean(Sales2020$gem_prijs)


t <- ggplot(SalesnoOutlierNL, aes(x = date))
t <- t + geom_line(aes(y = gem_prijs, colour = "Sales"))
t

