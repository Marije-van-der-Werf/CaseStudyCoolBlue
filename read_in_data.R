###############################################################################
# Script to read in the data
###############################################################################

#' Data and libraries
library(dplyr)
library(magrittr)
library(lubridate)
library(tidyr)

Competitor <- read.csv("Competitor Data - TV.csv", sep = ",", header = TRUE) %>% 
    mutate(date = as.Date(date))
Display <- read.csv("Display Data.csv", sep = ";", header = TRUE) %>% 
    mutate(Date = as.Date(substr(Date, 1, 10)))
Display <- rename(Display, date = Date, cost = Cost)
OtherMarketing <- read.csv("Other_Marketing_Data.csv", sep = ";", header = TRUE) %>% 
    mutate(date = as.Date(substr(date, 1, 10)))
Sales <- read.csv("Sales Data.csv", sep = ",", header = TRUE) %>% 
    mutate(date = as.Date(substr(invoice_date, 1, 10))) %>% 
    filter(date >= "2017-01-01") %>% 
    select(date, everything(), -invoice_date)
Social <- read.csv("Social Data.csv", sep = ",", header = TRUE) %>% 
    mutate(date = as.Date(date))

#' Clean the data
#' Round numbers
Display <- Display %>% 
    mutate(cost = round(cost, 2))
Social <- Social %>% 
    mutate(cost = round(cost, 2))
Sales <- Sales %>% 
    mutate(products_sold = round(products_sold, 0),
           distinct_orders = round(distinct_orders, 0))
OtherMarketing <- OtherMarketing %>% 
    mutate(delivered_emails = round(delivered_emails, 0),
           opened_emails = round(opened_emails, 0),
           clicks_emails = round(clicks_emails, 0),
           estimated_daily_sales_value = round(estimated_daily_sales_value, 0),
           searchads_cost = round(searchads_cost, 2),
           shoppingads_cost = round(shoppingads_cost, 2),
           tv_cost = round(tv_cost, 2),
           radio_cost = round(radio_cost, 2),
           ooh_cost = round(ooh_cost, 2))

#' Filter on mobile phones (product_type_id == 2093)
Display2093 <- Display %>% 
    filter(product_type_id == 2093)
OtherMarketing2093 <- OtherMarketing %>% 
    filter(key %in% c("2093_11", "2093_12", "2093_31", "2093_32", "2093_34"))
Sales2093 <- Sales %>% 
    filter(product_type_id == 2093)
Social2093 <- Social %>% 
    filter(product_type_id == 2093)

#' Dutch data, only mobile phones
DisplayNL <- Display2093 %>% 
    filter(subsidiary_id == 1)
OtherMarketingNL <- OtherMarketing2093 %>% 
    filter(key %in% c("2093_11"))
SalesNL <- Sales2093 %>% 
    filter(subsidiary_id == 1)
SocialNL <- Social2093 %>% 
    filter(subsidiaryid == 1)

#' Belgium data, only mobile phones
DisplayBE <- Display2093 %>%
    filter(subsidiary_id == 3,
           language_id == 1)
OtherMarketingBE <- OtherMarketing2093 %>%
    filter(key %in% c("2093_31", "2093_32"))
SalesBE <- Sales2093 %>% 
    filter(subsidiary_id == 3)
SocialBE<- Social2093 %>% 
    filter(subsidiaryid == 3,
           language_id == 1)

#' Remove variables which will not be used anymore
rm(Display2093, OtherMarketing2093, Sales2093, Social2093)
