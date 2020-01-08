###############################################################################
# Script voor data inlezen
###############################################################################

#' Data and libraries
library(dplyr)

Competitor <- read.csv("Competitor Data - TV.csv", sep = ",", header = TRUE)
Display <- read.csv("Display Data.csv", sep = ";", header = TRUE) %>% 
    mutate(date = substr(Date, 1, 10)) %>% 
    select(date, everything(), -Date)
OtherMarketing <- read.csv("Other_Marketing_Data.csv", sep = ";", header = TRUE) %>% 
    mutate(date = substr(date, 1, 10))
Sales <- read.csv("Sales Data.csv", sep = ",", header = TRUE) %>% 
    mutate(date = substr(invoice_date, 1, 10)) %>% 
    select(date, everything(), -invoice_date)
Social <- read.csv("Social Data.csv", sep = ",", header = TRUE)
