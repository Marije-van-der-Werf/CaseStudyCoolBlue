library(lubridate)
library(dplyr)
library(mice)
source("data_inlezen.R")
source("dummies.R")

#'We start with the data for NL only

#'We take the total display costs on one day
display_cost_perday <- DisplayNL %>% 
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

sales_perday <- SalesNL[order(SalesNL$date), c("date", "sales")]

#' Update modeldata: add display costs and sales  
modelData <- left_join(sales_perday,display_cost_perday, by = "date")

#'We take the total social costs on one day
social_cost_perday <- SocialNL %>% 
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

#' Update modeldata: add social costs
modelData <- left_join(modelData,social_cost_perday, by = "date")

#' Update modeldata: add search_ads costs and shopping_ads costs
adspervolume <- OtherMarketingNL$searchads_cost / OtherMarketingNL$search_volume
shoppingpervolume <- OtherMarketingNL$shoppingads_cost / OtherMarketingNL$search_volume
googleCosts <- data.frame(matrix(0,nrow(OtherMarketingNL),3))
names(googleCosts) <- cbind("date", "adspervolume", "shoppingpervolume")
googleCosts[,"date"] <- OtherMarketingNL$date
googleCosts[,"adspervolume"] <- adspervolume
googleCosts[, "shoppingpervolume"] <- shoppingpervolume
modelData <- left_join(modelData, googleCosts, by = "date")

modelData <- left_join(modelData,OtherMarketingNL[,c("date", "delivered_emails")], by = "date")

#' Update modeldata: add extrapolated competitor data
CompetitorNL <- Competitor %>% 
    filter(country == "Netherlands") %>% 
    mutate(Retailer = ifelse(brand %in% c("ALIEXPRESS", "ALTERNATE", "AMAZON", "AMAZON", "AMAZON MUSIC", "AO.NL", "BAX MUSIC", "BCC", "BLOKKER", "BOL.COM", "EXPERT", "FONQ.NL", "KREFEL", "MEDIA MARKT", "MEDIAMARKT", "VANDEN BORRE", "WEHKAMP.NL"), 1, 0),
           GRPRetailer = ifelse(Retailer == 1, GRP, 0),
           GRPNoRetailer = ifelse(Retailer == 0, GRP, 0)) %>% 
    spread(brand, GRP)

CompetitorNL[is.na(CompetitorNL)] <- 0

CompetitorNL <- CompetitorNL %>% 
    select(- c(channel, country, Retailer)) %>% 
    group_by(date) %>% 
    summarise_all(sum)

datums <- SalesNL %>% 
    select(date)

CompetitorNA <- datums %>% 
    left_join(CompetitorNL, by = "date")

set.seed(6)
ExtrapolerenRetailer <- mice(CompetitorNA %>% select(date, GRPRetailer), m = 1, method = "pmm", maxit = 1)
CompletedDataRetailer <- complete(ExtrapolerenRetailer, 1)

set.seed(12)
ExtrapolerenNoRetailer <- mice(CompetitorNA %>% select(date, GRPNoRetailer), m = 1, method = "pmm", maxit = 1)
CompletedDataNoRetailer <- complete(ExtrapolerenNoRetailer, 1)

CompetitorExtrapolated <- CompletedDataRetailer %>% 
    left_join(CompletedDataNoRetailer, by = "date")

modelData <- left_join(modelData, CompetitorExtrapolated, by = "date")

KNMI_data <- read.table("etmgeg_260.txt", header = TRUE, sep = ",", dec = ".")

#' Put dates from integer to date format and rename
dates <- as.character(KNMI_data$YYYYMMDD)
dates <- as.Date(dates, "%Y%m%d")
KNMI_data[,2]<-dates
KNMI_data %>% 
    mutate(YYYYMMDD = as.Date(YYYYMMDD))
names(KNMI_data)[2] <- "date"

#' Make sure temperature in full degrees (instead of 0.1)
KNMI_data$TX <- KNMI_data$TX/10
KNMI_data$SQ <- KNMI_data$SQ/10
KNMI_data$DR <- KNMI_data$DR/10

KNMIVA2017 <- KNMI_data %>% 
    filter(date >= "2017-01-01",
           date <= "2020-01-05"
    )

Neerslag = KNMIVA2017[, c('date', 'DR')]

modelData <- left_join(modelData, Neerslag, by = "date")

#' Update modeldata: add price index dummies
priceDummies <- data.frame(matrix(0,nrow(OtherMarketingNL),3))
priceDummies <- OtherMarketingNL[, c(1, 14, 15)]
names(priceDummies) <- cbind("date", "cheapest", "cheaperthanavg")
priceDummies$cheapest <- priceDummies$cheapest - min(priceDummies$cheapest)
priceDummies$cheaperthanavg <- priceDummies$cheaperthanavg - min(priceDummies$cheaperthanavg)

modelData <- left_join(modelData, priceDummies, by = "date")

#' Update modeldata: add marketing costs televisions and laptops
SpillOverDisplay <- Display %>% 
    filter(subsidiary_id == 1,
           product_type_id == c("2627", "17632")) %>% 
    group_by(date, product_type_id) %>% 
    summarise(costDisplay = sum(cost))

SpillOverSocial <- Social %>% 
    filter(subsidiaryid == 1,
           product_type_id == c("2627", "17632")) %>% 
    group_by(date, product_type_id) %>% 
    summarise(costSocial = sum(cost))

SpillOverOther <- OtherMarketing %>% 
    filter(key %in% c("17632_11", "2627_11")) %>% 
    mutate(key = as.numeric(substr(key, 1, nchar(as.character(key)) - 3))) %>% 
    group_by(date, key) %>% 
    summarise(costOther = sum(searchads_cost / search_volume, shoppingads_cost / search_volume, tv_cost, radio_cost, ooh_cost))

SpillOver <- SpillOverDisplay %>% 
    left_join(y = SpillOverSocial, by = c("date", "product_type_id")) %>% 
    left_join(y = SpillOverOther, by = c("date", "product_type_id" = "key")) %>% 
    mutate(product_type_id1 = product_type_id,
           product_type_id2 = product_type_id) %>% 
    spread(product_type_id, costDisplay) %>% 
    rename("17632Display" = "17632", "2627Display" = "2627") %>% 
    spread(product_type_id1, costSocial) %>% 
    rename("17632Social" = "17632", "2627Social" = "2627") %>% 
    spread(product_type_id2, costOther) %>% 
    rename("17632Other" = "17632", "2627Other" = "2627") 
SpillOver[is.na(SpillOver)] <- 0

SpillOver <- SpillOver %>% 
    group_by(date) %>% 
    summarise_all(sum)

SpillOverAllTV <- OtherMarketing %>% 
    filter(key %in% c("17632_11", "2093_11", "2627_11", "2675_11", "2676_11", "2677_11", "2678_11", "2679_11", "8804_11")) %>% 
    mutate(key = as.numeric(substr(key, 1, nchar(as.character(key)) - 3))) %>% 
    group_by(date) %>% 
    summarise(allTVcost = sum(tv_cost))

SpillOver <- SpillOver %>% 
    left_join(SpillOverAllTV, by = "date")

modelData <- modelData %>% 
    left_join(SpillOver, by = "date")

#' Update modeldata: add weekdaydummy
modelData <- left_join(modelData, WeekdayDummy, by = "date")

#' Update modeldata: add month dummy
modelData <- left_join(modelData, MonthDummy, by = "date")

#' Update modeldata: add seasonal dummy
modelData <- left_join(modelData, SeasonalDummy, by = "date")

#' Update modeldata: add partyDays dummy
modelData <- modelData %>% 
    left_join(FeestdagenNL, by = "date")

#' Remove data from environment that is now in one big table
rm(sales_perday, display_cost_perday, social_cost_perday, priceDummies, CompetitorNA, CompetitorExtrapolated, ExtrapolerenNoRetailer, ExtrapolerenRetailer, CompletedDataNoRetailer, CompletedDataRetailer, SpillOver, SpillOverOther, SpillOverSocial, SpillOverDisplay)

modelData[is.na(modelData)] <- 0