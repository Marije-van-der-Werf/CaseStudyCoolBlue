library(lubridate)
library(dplyr)
library(mice)
source("data_inlezen.R")
source("dummies.R")

display_cost_perday <- DisplayBE %>% 
    mutate(cost1 = cost,
           cost2 = cost,
           cost3 = cost,
           cost4 = cost) %>% 
    spread(campaign_type, cost1) %>% 
    rename(sales_campaign = Sales, media = Media, visibility = Visibility) %>% 
    spread(strategy, cost2) %>% 
    rename(prospecting = Prospecting, retargeting = Retargeting, unknown_strategy = unknown) %>% 
    spread(device, cost3) %>% 
    rename(mobile = Mobile, desktop = Desktop, tablet = Tablet, unknown_device = unknown) %>% 
    spread(buying_method, cost4) %>% 
    rename(programmatic = Programmatic, buy_direct = "Buy Direct") %>% 
    select(date, sales_campaign, media, visibility, prospecting, retargeting, unknown_strategy, mobile, desktop, tablet, unknown_device, programmatic, buy_direct) 

display_cost_perday[is.na(display_cost_perday)] <- 0

display_cost_perday <- display_cost_perday %>% 
    group_by(date) %>% 
    summarise_all(sum) %>% 
    ungroup() %>% 
    mutate(sales_campaign = sales_campaign / 4,
           media = media / 4,
           visibility = visibility / 4,
           prospecting = prospecting / 4,
           retargeting = retargeting / 4,
           unknown_strategy = unknown_strategy / 4,
           mobile = mobile / 4,
           desktop = desktop / 4,
           tablet = tablet / 4,
           unknown_device = unknown_device / 4,
           programmatic = programmatic / 4,
           buy_direct = buy_direct / 4)

sales_perday <- SalesBE[order(SalesBE$date), c("date", "sales")]

#' Update Model data: add display costs and sales  
modelDataBE <- left_join(sales_perday,display_cost_perday, by = "date")

#Update met franse taal
display_cost_perday_BEFR <- DisplayBEFR %>% 
    mutate(cost1 = cost,
           cost2 = cost,
           cost3 = cost,
           cost4 = cost) %>% 
    spread(campaign_type, cost1) %>% 
    rename(sales_campaign_FR = Sales, media_FR = Media) %>% 
    spread(strategy, cost2) %>% 
    rename(prospecting_FR = Prospecting, retargeting_FR = Retargeting) %>% 
    spread(device, cost3) %>% 
    rename(desktop_FR = Desktop, unknown_device_FR = unknown) %>% 
    spread(buying_method, cost4) %>% 
    rename(programmatic_FR = Programmatic) %>% 
    select(date, sales_campaign_FR, media_FR, prospecting_FR, retargeting_FR, desktop_FR, unknown_device_FR, programmatic_FR) 

display_cost_perday_BEFR[is.na(display_cost_perday_BEFR)] <- 0

display_cost_perday_BEFR <- display_cost_perday_BEFR %>% 
    group_by(date) %>% 
    summarise_all(sum) %>% 
    ungroup() %>% 
    mutate(sales_campaign_FR = sales_campaign_FR / 4,
           media_FR = media_FR / 4,
           prospecting_FR = prospecting_FR / 4,
           retargeting_FR = retargeting_FR / 4,
           desktop_FR = desktop_FR / 4,
           unknown_device_FR = unknown_device_FR / 4,
           programmatic_FR = programmatic_FR / 4,
    )

#' Update Model data: add display costs and sales  
modelDataBE <- left_join(modelDataBE, display_cost_perday_BEFR, by = "date")
modelDataBE[is.na(modelDataBE)] <- 0

#'We take the total social costs on one day
social_cost_perday <- SocialBE %>% 
    mutate(cost1 = cost,
           cost2 = cost,
           cost3 = cost,
           cost4 = cost) %>% 
    spread(goal, cost1) %>% 
    rename(sales_goalS = Sales, mediaS = Media, visibilityS = Visibility) %>% 
    spread(strategy, cost2) %>% 
    rename(prospectingS = Prospecting, retargetingS = Retargeting, unknown_strategyS = error) %>% 
    spread(device, cost3) %>% 
    rename(mobileS = Mobile, desktopS = Desktop, mobileDesktopS = "Mobile / Desktop") %>% 
    spread(channel, cost4) %>% 
    rename(FacebookAndInstagram = "Facebook and Instagram") %>% 
    select(date, sales_goalS, mediaS, visibilityS, prospectingS, retargetingS, unknown_strategyS, mobileS, desktopS, mobileDesktopS, Instagram, Facebook, FacebookAndInstagram) 

social_cost_perday[is.na(social_cost_perday)] <- 0

social_cost_perday <- social_cost_perday %>% 
    group_by(date) %>% 
    summarise_all(sum) %>% 
    ungroup() %>% 
    mutate(sales_goalS = sales_goalS / 4,
           mediaS = mediaS / 4,
           visibilityS = visibilityS / 4,
           prospectingS = prospectingS / 4,
           retargetingS = retargetingS / 4,
           unknown_strategyS = unknown_strategyS / 4,
           mobileS = mobileS / 4,
           desktopS = desktopS / 4,
           mobileDesktopS = mobileDesktopS / 4,
           Instagram = Instagram / 4,
           Facebook = Facebook / 4,
           FacebookAndInstagram = FacebookAndInstagram / 4)

#' Update Model data: add social costs
modelDataBE <- left_join(modelDataBE,social_cost_perday, by = "date")


#'We take the total social costs on one day (FRANCE)
social_cost_perday_FR <- SocialBEFR %>% 
    mutate(cost1 = cost,
           cost2 = cost,
           cost3 = cost,
           cost4 = cost) %>% 
    spread(goal, cost1) %>% 
    rename(sales_goalS_FR = Sales, mediaS_FR = Media) %>% 
    spread(strategy, cost2) %>% 
    rename(prospectingS_FR = Prospecting, retargetingS_FR = Retargeting) %>% 
    spread(device, cost3) %>% 
    rename(mobileS_FR = Mobile, desktopS_FR = Desktop, mobileDesktopS_FR = "Mobile / Desktop") %>% 
    spread(channel, cost4) %>% 
    rename(Facebook_FR = Facebook, Instagram_FR = Instagram, FacebookAndInstagram_FR = "Facebook and Instagram") %>% 
    select(date, sales_goalS_FR, mediaS_FR, prospectingS_FR, retargetingS_FR, mobileS_FR, desktopS_FR, mobileDesktopS_FR, Instagram_FR, Facebook_FR, FacebookAndInstagram_FR) 

social_cost_perday_FR[is.na(social_cost_perday_FR)] <- 0

social_cost_perday_FR <- social_cost_perday_FR %>% 
    group_by(date) %>% 
    summarise_all(sum) %>% 
    ungroup() %>% 
    mutate(sales_goalS_FR = sales_goalS_FR / 4,
           mediaS_FR = mediaS_FR / 4,
           prospectingS_FR = prospectingS_FR / 4,
           retargetingS_FR = retargetingS_FR / 4,
           mobileS_FR = mobileS_FR / 4,
           desktopS_FR = desktopS_FR / 4,
           mobileDesktopS_FR = mobileDesktopS_FR / 4,
           Instagram_FR = Instagram_FR / 4,
           Facebook_FR = Facebook_FR / 4,
           FacebookAndInstagram_FR = FacebookAndInstagram_FR / 4)

#' Update Model data: add social costs
modelDataBE <- left_join(modelDataBE,social_cost_perday_FR, by = "date")




#' Update Model data: add search_ads costs and shopping_ads costs
adspervolume <- OtherMarketingBE$searchads_cost / OtherMarketingBE$search_volume
shoppingpervolume <- OtherMarketingBE$shoppingads_cost / OtherMarketingBE$search_volume
googleCosts <- data.frame(matrix(0,nrow(OtherMarketingBE),3))
names(googleCosts) <- cbind("date", "adspervolume", "shoppingpervolume")
googleCosts[,"date"] <- OtherMarketingBE$date
googleCosts[,"adspervolume"] <- adspervolume
googleCosts[, "shoppingpervolume"] <- shoppingpervolume
modelDataBE <- left_join(modelDataBE, googleCosts, by = "date")

modelDataBE <- left_join(modelDataBE,OtherMarketingBE[,c("date", "delivered_emails")], by = "date")


#' Add Competitor Data
CompetitorBE <- Competitor %>% 
    filter(country == "Belgie NL" | country == "Belgie FR") %>% 
    mutate(Retailer = ifelse(brand %in% c("ALIEXPRESS", "ALTERNATE", "AMAZON", "AMAZON", "AMAZON MUSIC", "AO.NL", "BAX MUSIC", "BCC", "BLOKKER", "BOL.COM", "EXPERT", "FONQ.NL", "KREFEL", "MEDIA MARKT", "MEDIAMARKT", "VANDEN BORRE", "WEHKAMP.NL"), 1, 0),
           GRPRetailer = ifelse(Retailer == 1, GRP, 0),
           GRPNoRetailer = ifelse(Retailer == 0, GRP, 0)) %>% 
    spread(brand, GRP)

CompetitorBE[is.na(CompetitorBE)] <- 0

CompetitorBE <- CompetitorBE %>% 
    select(- c(channel, country, Retailer)) %>% 
    group_by(date) %>% 
    summarise_all(sum)

datums <- SalesBE %>% 
    select(date)

CompetitorNA <- datums %>% 
    left_join(CompetitorBE, by = "date")

set.seed(6)
ExtrapolerenRetailer <- mice(CompetitorNA %>% select(date, GRPRetailer), m = 1, method = "pmm", maxit = 1)
CompletedDataRetailer <- complete(ExtrapolerenRetailer, 1)

set.seed(12)
ExtrapolerenNoRetailer <- mice(CompetitorNA %>% select(date, GRPNoRetailer), m = 1, method = "pmm", maxit = 1)
CompletedDataNoRetailer <- complete(ExtrapolerenNoRetailer, 1)

CompetitorExtrapolated <- CompletedDataRetailer %>% 
    left_join(CompletedDataNoRetailer, by = "date")

modelDataBE <- left_join(modelDataBE, CompetitorExtrapolated, by = "date")

#Weer in Belgie erin
KNMI_dataBE <- read.table("etmgeg_380.txt", header = TRUE, sep = ",", dec = ".")

#' Put dates from integer to date format and rename
dates<- as.character(KNMI_dataBE$YYYYMMDD)
dates<-as.Date(dates, "%Y%m%d")
KNMI_dataBE[,2]<-dates
KNMI_dataBE%>% 
    mutate(YYYYMMDD = as.Date(YYYYMMDD))
names(KNMI_dataBE)[2] <- "date"

#' Make sure temperature in full degrees (instead of 0.1)
KNMI_dataBE$TX <- KNMI_dataBE$TX/10
KNMI_dataBE$SQ <- KNMI_dataBE$SQ/10
KNMI_dataBE$DR <- KNMI_dataBE$DR/10

KNMIVA2017 <- KNMI_dataBE %>% 
    filter(date >= "2017-01-01",
           date <= "2020-01-05"
    )

Neerslag = KNMIVA2017[, c('date', 'DR')]

modelDataBE <- left_join(modelDataBE, Neerslag, by = "date")

#' Update Model data: add price index dummies
priceDummies <- data.frame(matrix(0,nrow(OtherMarketingBE),3))
priceDummies <- OtherMarketingBE[, c(1, 14, 15)]
names(priceDummies) <- cbind("date", "cheapest", "cheaperthanavg")
priceDummies$cheapest <- priceDummies$cheapest - min(priceDummies$cheapest)
priceDummies$cheaperthanavg <- priceDummies$cheaperthanavg - min(priceDummies$cheaperthanavg)

modelDataBE <- left_join(modelDataBE, priceDummies, by = "date")

#' Update Model data: add weekdaydummy
modelDataBE <- left_join(modelDataBE, WeekdayDummy, by = "date")

#' Update Model data: add month dummy
modelDataBE <- left_join(modelDataBE, MonthDummy, by = "date")

#' Update Model data: add seasonal dummy
modelDataBE <- left_join(modelDataBE, SeasonalDummy, by = "date")

#Update Model data: add PartyDays dummy
modelDataBE <- modelDataBE %>% 
    left_join(FeestdagenBE, by = "date")



#' Remove data from environment that is now in one big table
rm(sales_perday)
rm(display_cost_perday)
rm(social_cost_perday)
rm(display_cost_perday_BEFR)
rm(social_cost_perday_FR)
rm(priceDummies)

modelDataBE[is.na(modelDataBE)] <- 0
