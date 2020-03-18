Lisanne: #########################################################################
# Script KNMI weather data 
#########################################################################
library(dplyr)
library(magrittr)
library(base)
library(ggplot2)

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

#' Make sure duration of rain is in minutes
KNMI_data$DR <- KNMI_data$DR * 6

<<<<<<< HEAD
=======

>>>>>>> 72d68fa05085dd71f7369e205601047c003fb7f8
Sales2093 <- Sales2093 %>% 
    filter(subsidiary_id == 1)

SalesandWeather <- left_join(Sales2093, KNMI_data, by = "date")

<<<<<<< HEAD

=======
#############################################################################
#' This is to make some plots
SalesandWeather <- SalesandWeather[SalesandWeather$products_sold < 1000,]
SalesandWeather <- SalesandWeather[SalesandWeather$date < "2017-12-31",]
>>>>>>> 72d68fa05085dd71f7369e205601047c003fb7f8
p <- ggplot(SalesandWeather, aes(x = date))
p <- p + geom_line(aes(y = products_sold, colour = "Products"))

# adding the relative humidity data, transformed to match roughly the range of the temperature
<<<<<<< HEAD
p <- p + geom_line(aes(y = TG*200, colour = "Temp"))

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./200, name = "Relative"))
=======
p <- p + geom_line(aes(y = DR*4, colour = "RD"))

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./4, name = "Relative"))
>>>>>>> 72d68fa05085dd71f7369e205601047c003fb7f8

# modifying colours and theme options
p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "Products sold",
              x = "Date",
              colour = "Parameter")
p <- p + theme(legend.position = c(0.8, 0.9))
p

<<<<<<< HEAD
SalesandWeather <- SalesandWeather[SalesandWeather$products_sold < 1000,]

t <- ggplot(SalesandWeather, aes(x = date))
t <- t + geom_line(aes(y = sales, colour = "Sales"))
t
















=======
lm(SalesandWeather$products_sold~SalesandWeather$TG)
>>>>>>> 72d68fa05085dd71f7369e205601047c003fb7f8
