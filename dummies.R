library(lubridate)
s = dim(OtherMarketingNL)

WeekdayDummy = as.data.frame(matrix(0, nrow = s[1], ncol = 7))
colnames(WeekdayDummy) <- c("date", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
WeekdayDummy[,1] <- OtherMarketingNL$date


# Monday is base
for (i in 1:s[1]){
    if (wday(OtherMarketingNL[i,1], label = TRUE) == "di"){
        WeekdayDummy[i,2] = 1
    }
    if (wday(OtherMarketingNL[i,1], label = TRUE) == "wo"){
        WeekdayDummy[i,3] = 1
    }
    if (wday(OtherMarketingNL[i,1], label = TRUE) == "do"){
        WeekdayDummy[i,4] = 1
    }
    if (wday(OtherMarketingNL[i,1], label = TRUE) == "vr"){
        WeekdayDummy[i,5] = 1
    }
    if (wday(OtherMarketingNL[i,1], label = TRUE) == "za"){
        WeekdayDummy[i,6] = 1
    }
    if (wday(OtherMarketingNL[i,1], label = TRUE) == "zo"){
        WeekdayDummy[i,7] = 1
    }
}

MonthDummy = as.data.frame(matrix(0, nrow = s[1], ncol = 12))
colnames(MonthDummy) <- c("date", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
MonthDummy[,1] <- OtherMarketingNL$date

# January is base
for (i in 1:s[1]){
    if (month(MonthDummy[i,1]) == "2"){
        MonthDummy[i,2] = 1
    }
    if (month(MonthDummy[i,1]) == "3"){
        MonthDummy[i,3] = 1
    }
    if (month(MonthDummy[i,1]) == "4"){
        MonthDummy[i,4] = 1
    }
    if (month(MonthDummy[i,1]) == "5"){
        MonthDummy[i,5] = 1
    }
    if (month(MonthDummy[i,1]) == "6"){
        MonthDummy[i,6] = 1
    }
    if (month(MonthDummy[i,1]) == "7"){
        MonthDummy[i,7] = 1
    }
    if (month(MonthDummy[i,1]) == "8"){
        MonthDummy[i,8] = 1
    }
    if (month(MonthDummy[i,1]) == "9"){
        MonthDummy[i,9] = 1
    }
    if (month(MonthDummy[i,1]) == "10"){
        MonthDummy[i,10] = 1
    }
    if (month(MonthDummy[i,1]) == "11"){
        MonthDummy[i,11] = 1
    }
    if (month(MonthDummy[i,1]) == "12"){
        MonthDummy[i,12] = 1
    }
}

#De lente begint op 21 maart, de zomer begint op 21 juni, de herfst op 21 september en de winter op 21 december
SeasonalDummy = as.data.frame(matrix(0, nrow = s[1], ncol = 4))
colnames(SeasonalDummy) <- c("date", "Spring", "Summer", "Fall")
SeasonalDummy[,1] <- OtherMarketingNL$date

#for (i in 1:s[1]){
#    if (month(OtherMarketingNL[i,1]) %in% "4":"6"){
#        SeasonalDummy[i,2] = 1
#    }
#    if (month(OtherMarketingNL[i,1]) %in% "7":"9"){
#        SeasonalDummy[i,3] = 1
#    }
#    if (month(OtherMarketingNL[i,1]) %in% "10":"12"){
#        SeasonalDummy[i,4] = 1
#    }
#}

#Winter is base
for (i in 1:s[1]){
    if (SeasonalDummy[i,1] >= "2017-03-21" & SeasonalDummy[i,1] <= "2017-06-20"){
        SeasonalDummy[i,2] = 1
    }
    if (SeasonalDummy[i,1] >= "2018-03-21" & SeasonalDummy[i,1] <= "2018-06-20"){
        SeasonalDummy[i,2] = 1
    }
    if (SeasonalDummy[i,1] >= "2019-03-21" & SeasonalDummy[i,1] <= "2019-06-20"){
        SeasonalDummy[i,2] = 1
    }
    if (SeasonalDummy[i,1] >= "2017-06-21" & SeasonalDummy[i,1] <= "2017-09-20"){
        SeasonalDummy[i,3] = 1
    }
    if (SeasonalDummy[i,1] >= "2018-06-21" & SeasonalDummy[i,1] <= "2018-09-20"){
        SeasonalDummy[i,3] = 1
    }
    if (SeasonalDummy[i,1] >= "2019-06-21" & SeasonalDummy[i,1] <= "2019-09-20"){
        SeasonalDummy[i,3] = 1
    }
    if (SeasonalDummy[i,1] >= "2017-09-21" & SeasonalDummy[i,1] <= "2017-12-20"){
        SeasonalDummy[i,4] = 1
    }
    if (SeasonalDummy[i,1] >= "2018-09-21" & SeasonalDummy[i,1] <= "2018-12-20"){
        SeasonalDummy[i,4] = 1
    }
    if (SeasonalDummy[i,1] >= "2019-09-21" & SeasonalDummy[i,1] <= "2019-12-20"){
        SeasonalDummy[i,4] = 1
    }
}

#' Dummy voor retailer in Competitor dataframe
Competitor <- Competitor %>% 
    mutate(Retailer = ifelse(brand %in% c("ALIEXPRESS", "ALTERNATE", "AMAZON", "AMAZON", "AMAZON MUSIC", "AO.NL", "BAX MUSIC", "BCC", "BLOKKER", "BOL.COM", "EXPERT", "FONQ.NL", "KREFEL", "MEDIA MARKT", "MEDIAMARKT", "VANDEN BORRE", "WEHKAMP.NL"), 1, 0))
