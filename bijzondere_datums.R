###############################################################################
# Script voor bijzondere datums ontdekken (NL)
###############################################################################

library(ggplot2)
source('data_inlezen.R')

Sales2017 <- Sales2093 %>% 
    filter(date <= "2017-12-31",
           subsidiary_id == 1,
           products_sold >= 1000) %>% 
    mutate(gem_prijs = sales / products_sold)

Sales2018 <- Sales2093 %>% 
    filter(date >= "2018-01-01",
           date <= "2018-12-31",
           subsidiary_id == 1,
           products_sold >= 1000) %>% 
    mutate(gem_prijs = sales / products_sold)

Sales2019 <- Sales2093 %>% 
    filter(date >= "2019-01-01",
           date <= "2019-12-31",
           subsidiary_id == 1,
           products_sold >= 1000) %>% 
    mutate(gem_prijs = sales / products_sold)

Sales2017Alles <- Sales2093 %>% 
    filter(date <= "2017-12-31",
           subsidiary_id == 1)

Sales2018Alles <- Sales2093 %>% 
    filter(date >= "2018-01-01",
           date <= "2018-12-31",
           subsidiary_id == 1)

Sales2019Alles <- Sales2093 %>% 
    filter(date >= "2019-01-01",
           date <= "2019-12-31",
           subsidiary_id == 1)

ggplot(data = Sales2017Alles, aes(x = date, y = products_sold)) +
    geom_line()
