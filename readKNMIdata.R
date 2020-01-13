#########################################################################
# Script KNMI weather data 
#########################################################################
library(dplyr)
library(magrittr)

KNMI_data <- read.table("KNMI_weather.txt", header = TRUE, sep = ",", dec = ".")

#' Put dates from integer to date format and rename
dates<- as.character(KNMI_data$YYYYMMDD)
dates<-as.Date(dates, "%Y%m%d")
KNMI_data[,2]<-dates
KNMI_data%>% 
    mutate(YYYYMMDD = as.Date(YYYYMMDD))
names(KNMI_data)[2] <- "date"

#' Make sure temperature in full degrees (instead of 0.1)
KNMI_data$TG <- KNMI_data$TG/10

