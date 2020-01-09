###############################################################################
# Script voor data inlezen
###############################################################################

#' Data and libraries
library(dplyr)
library(magrittr)

Competitor <- read.csv("Competitor Data - TV.csv", sep = ",", header = TRUE)
Display <- read.csv("Display Data.csv", sep = ";", header = TRUE) %>% 
    mutate(Date = substr(Date, 1, 10))
Display <- rename(Display, date = Date, cost = Cost)
OtherMarketing <- read.csv("Other_Marketing_Data.csv", sep = ";", header = TRUE) %>% 
    mutate(date = substr(date, 1, 10))
Sales <- read.csv("Sales Data.csv", sep = ",", header = TRUE) %>% 
    mutate(date = substr(invoice_date, 1, 10)) %>% 
    select(date, everything(), -invoice_date)
Social <- read.csv("Social Data.csv", sep = ",", header = TRUE)

#' Data opschonen
OtherMarketing2 <- OtherMarketing %>% 
    mutate(tv_cost = as.numeric(as.character(tv_cost)),
           tv_cost = round(tv_cost, 0),
           radio_cost = as.numeric(as.character(radio_cost)),
           radio_cost = round(radio_cost, 0))



OtherMarketing <- OtherMarketing %>% 
    mutate_at(vars(tv_cost, radio_cost, ooh_cost, search_volume, estimated_daily_sales_value), funs(as.numeric()))
