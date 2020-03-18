###############################################################################
# Script for adding dummies
###############################################################################

#' Data and libraries
library(lubridate)
source("read_in_data.R")

s = dim(OtherMarketingNL)

#' Add Weekday dummies
WeekdayDummy = as.data.frame(matrix(0, nrow = s[1], ncol = 7))
colnames(WeekdayDummy) <- c("date", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
WeekdayDummy[,1] <- OtherMarketingNL$date

#' Monday is base
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

#' Add month dummies
MonthDummy = as.data.frame(matrix(0, nrow = s[1], ncol = 12))
colnames(MonthDummy) <- c("date", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
MonthDummy[,1] <- OtherMarketingNL$date

#' January is base
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

#' Add seasonal dummies
#' Spring starts on March 21, summer on June 21, fall on September 21 and winter on December 21
SeasonalDummy = as.data.frame(matrix(0, nrow = s[1], ncol = 4))
colnames(SeasonalDummy) <- c("date", "Spring", "Summer", "Fall")
SeasonalDummy[,1] <- OtherMarketingNL$date

#' Winter is base
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

#' Dummies for specific holidays/actions, based on dates
Holidays <- SalesNL %>% 
    select(date) %>% 
    mutate(BF17 = ifelse(date == "2017-11-23", 1, 0), #' 2017 thursday up to and including monday
           BF17 = ifelse(date == "2017-11-24", 1, BF17),
           BF17 = ifelse(date == "2017-11-25", 1, BF17),
           BF17 = ifelse(date == "2017-11-26", 1, BF17),
           BF17 = ifelse(date == "2017-11-27", 1, BF17),
           BF18 = ifelse(date == "2018-11-22", 1, 0), #' 2018 thursday up to and including monday
           BF18 = ifelse(date == "2018-11-23", 1, BF18),
           BF18 = ifelse(date == "2018-11-24", 1, BF18),
           BF18 = ifelse(date == "2018-11-25", 1, BF18),
           BF18 = ifelse(date == "2018-11-26", 1, BF18),
           BF19 = ifelse(date == "2019-11-25", 1, 0), #' 2019 monday up to and including monday
           BF19 = ifelse(date == "2019-11-26", 1, BF19),
           BF19 = ifelse(date == "2019-11-27", 1, BF19),
           BF19 = ifelse(date == "2019-11-28", 1, BF19),
           BF19 = ifelse(date == "2019-11-29", 1, BF19),
           BF19 = ifelse(date == "2019-11-30", 1, BF19),
           BF19 = ifelse(date == "2019-12-01", 1, BF19),
           BF19 = ifelse(date == "2019-12-01", 1, BF19), 
           iPhoneLaunch = ifelse(date == "2017-09-18", 1, 0), #' in most cases the day of the availability and the four days before
           iPhoneLaunch = ifelse(date == "2017-09-19", 1, iPhoneLaunch),
           iPhoneLaunch = ifelse(date == "2017-09-20", 1, iPhoneLaunch),
           iPhoneLaunch = ifelse(date == "2017-09-21", 1, iPhoneLaunch),
           iPhoneLaunch = ifelse(date == "2017-09-22", 1, iPhoneLaunch),
           iPhoneLaunch = ifelse(date == "2018-09-21", 1, iPhoneLaunch),
           iPhoneLaunch = ifelse(date == "2018-10-22", 1, iPhoneLaunch),
           iPhoneLaunch = ifelse(date == "2018-10-23", 1, iPhoneLaunch),
           iPhoneLaunch = ifelse(date == "2018-10-24", 1, iPhoneLaunch),
           iPhoneLaunch = ifelse(date == "2018-10-25", 1, iPhoneLaunch),
           iPhoneLaunch = ifelse(date == "2018-10-26", 1, iPhoneLaunch),
           iPhoneLaunch = ifelse(date == "2019-09-16", 1, iPhoneLaunch),
           iPhoneLaunch = ifelse(date == "2019-09-17", 1, iPhoneLaunch),
           iPhoneLaunch = ifelse(date == "2019-09-18", 1, iPhoneLaunch),
           iPhoneLaunch = ifelse(date == "2019-09-19", 1, iPhoneLaunch),
           iPhoneLaunch = ifelse(date == "2019-09-20", 1, iPhoneLaunch),
           BTWdeals = ifelse(date == "2017-01-25", 1, 0), #' the dates correspond with an action from competitor MediaMarkt
           BTWdeals = ifelse(date == "2017-01-26", 1, BTWdeals),
           BTWdeals = ifelse(date == "2017-01-27", 1, BTWdeals),
           BTWdeals = ifelse(date == "2017-01-28", 1, BTWdeals),
           BTWdeals = ifelse(date == "2017-01-29", 1, BTWdeals),
           BTWdeals = ifelse(date == "2018-01-25", 1, BTWdeals),
           BTWdeals = ifelse(date == "2018-01-26", 1, BTWdeals),
           BTWdeals = ifelse(date == "2018-01-27", 1, BTWdeals),
           BTWdeals = ifelse(date == "2018-01-28", 1, BTWdeals),
           BTWdeals = ifelse(date == "2019-01-21", 1, BTWdeals),
           BTWdeals = ifelse(date == "2019-01-22", 1, BTWdeals),
           BTWdeals = ifelse(date == "2019-01-23", 1, BTWdeals),
           BTWdeals = ifelse(date == "2019-01-24", 1, BTWdeals),
           BTWdeals = ifelse(date == "2019-01-25", 1, BTWdeals),
           BTWdeals = ifelse(date == "2019-01-26", 1, BTWdeals),
           BTWdeals = ifelse(date == "2019-01-27", 1, BTWdeals),
           ChristmasPresents = ifelse(date == "2017-12-16", 1, 0), #' buying Christmas presents (December 16 up to and including December 23)
           ChristmasPresents = ifelse(date == "2017-12-17", 1, ChristmasPresents),
           ChristmasPresents = ifelse(date == "2017-12-18", 1, ChristmasPresents),
           ChristmasPresents = ifelse(date == "2017-12-19", 1, ChristmasPresents),
           ChristmasPresents = ifelse(date == "2017-12-20", 1, ChristmasPresents),
           ChristmasPresents = ifelse(date == "2017-12-21", 1, ChristmasPresents),
           ChristmasPresents = ifelse(date == "2017-12-22", 1, ChristmasPresents),
           ChristmasPresents = ifelse(date == "2017-12-23", 1, ChristmasPresents),
           ChristmasPresents = ifelse(date == "2018-12-16", 1, ChristmasPresents),
           ChristmasPresents = ifelse(date == "2018-12-17", 1, ChristmasPresents),
           ChristmasPresents = ifelse(date == "2018-12-18", 1, ChristmasPresents),
           ChristmasPresents = ifelse(date == "2018-12-19", 1, ChristmasPresents),
           ChristmasPresents = ifelse(date == "2018-12-20", 1, ChristmasPresents),
           ChristmasPresents = ifelse(date == "2018-12-21", 1, ChristmasPresents),
           ChristmasPresents = ifelse(date == "2018-12-22", 1, ChristmasPresents),
           ChristmasPresents = ifelse(date == "2018-12-23", 1, ChristmasPresents),
           ChristmasPresents = ifelse(date == "2019-12-16", 1, ChristmasPresents),
           ChristmasPresents = ifelse(date == "2019-12-17", 1, ChristmasPresents),
           ChristmasPresents = ifelse(date == "2019-12-18", 1, ChristmasPresents),
           ChristmasPresents = ifelse(date == "2019-12-19", 1, ChristmasPresents),
           ChristmasPresents = ifelse(date == "2019-12-20", 1, ChristmasPresents),
           ChristmasPresents = ifelse(date == "2019-12-21", 1, ChristmasPresents),
           ChristmasPresents = ifelse(date == "2019-12-22", 1, ChristmasPresents),
           ChristmasPresents = ifelse(date == "2019-12-23", 1, ChristmasPresents),
           ChristianHoliday = ifelse(date == "2017-04-15", 1, 0), #' the day before Easter/Pentecost/Christmas and First easterday/Whit Sunday/first Christmas day
           ChristianHoliday = ifelse(date == "2017-04-16", 1, ChristianHoliday),
           ChristianHoliday = ifelse(date == "2017-06-03", 1, ChristianHoliday),
           ChristianHoliday = ifelse(date == "2017-06-04", 1, ChristianHoliday),
           ChristianHoliday = ifelse(date == "2017-12-24", 1, ChristianHoliday),
           ChristianHoliday = ifelse(date == "2017-12-25", 1, ChristianHoliday),
           ChristianHoliday = ifelse(date == "2018-03-31", 1, ChristianHoliday),
           ChristianHoliday = ifelse(date == "2018-04-01", 1, ChristianHoliday),
           ChristianHoliday = ifelse(date == "2018-05-19", 1, ChristianHoliday),
           ChristianHoliday = ifelse(date == "2018-05-20", 1, ChristianHoliday),
           ChristianHoliday = ifelse(date == "2018-12-24", 1, ChristianHoliday),
           ChristianHoliday = ifelse(date == "2018-12-25", 1, ChristianHoliday),
           ChristianHoliday = ifelse(date == "2019-04-20", 1, ChristianHoliday),
           ChristianHoliday = ifelse(date == "2019-04-21", 1, ChristianHoliday),
           ChristianHoliday = ifelse(date == "2019-06-08", 1, ChristianHoliday),
           ChristianHoliday = ifelse(date == "2019-06-09", 1, ChristianHoliday),
           ChristianHoliday = ifelse(date == "2019-12-24", 1, ChristianHoliday),
           ChristianHoliday = ifelse(date == "2019-12-25", 1, ChristianHoliday),
           Kingsday = ifelse(date == "2017-04-27", 1, 0),
           Kingsday = ifelse(date == "2018-04-27", 1, Kingsday),
           Kingsday = ifelse(date == "2019-04-27", 1, Kingsday),
           NewYears = ifelse(date == "2017-01-01", 1, 0),
           NewYears = ifelse(date == "2017-12-31", 1, NewYears),
           NewYears = ifelse(date == "2018-01-01", 1, NewYears),
           NewYears = ifelse(date == "2018-12-31", 1, NewYears),
           NewYears = ifelse(date == "2019-01-01", 1, NewYears),
           NewYears = ifelse(date == "2019-12-31", 1, NewYears),
           NewYears = ifelse(date == "2020-01-01", 1, NewYears),
           WinterSolden = ifelse(date == "2017-01-03", 1, 0), # Wintersolden is from January 3 up to and including January 31, but only the first two days there are more sales
           WinterSolden = ifelse(date == "2017-01-04", 1, WinterSolden),
           WinterSolden = ifelse(date == "2018-01-03", 1, WinterSolden),
           WinterSolden = ifelse(date == "2018-01-04", 1, WinterSolden),
           WinterSolden = ifelse(date == "2019-01-03", 1, WinterSolden),
           WinterSolden = ifelse(date == "2019-01-04", 1, WinterSolden))

HolidaysNL <- Holidays %>% 
    select(date, BF17, BF18, BF19, iPhoneLaunch, BTWdeals, ChristmasPresents, ChristianHoliday, Kingsday, NewYears)

HolidaysBE <- Holidays %>%
    select(date, BF17, BF18, BF19, ChristmasPresents, ChristianHoliday, NewYears, WinterSolden)

#' Remove variables which will not be used anymore
rm(Holidays)
rm(i, s)
