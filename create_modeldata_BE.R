###################################################################################################
# Script voor create_modeldata na Coolblue gesprek voor Belgie
###################################################################################################

library(lubridate)
library(dplyr)
library(mice)
source("data_inlezen.R")
source("dummies.R")

#'We start with the data for BE only
rm(Display2093, DisplayBEFR, DisplayNL, FeestdagenNL, OtherMarketing2093, OtherMarketingNL, Sales2093, SalesNL, Social2093, SocialBEFR, SocialNL)

#'We take the total display costs on one day
Display_per_combi <- DisplayBE %>% 
    mutate(D_Sa_De_Pr = ifelse(campaign_type %in% c("Sales", "Visibility") & device == "Desktop" & strategy == "Prospecting", cost, 0),
           D_Sa_De_Re = ifelse(campaign_type %in% c("Sales", "Visibility") & device == "Desktop" & strategy == "Retargeting", cost, 0),
           D_Sa_De_un = ifelse(campaign_type %in% c("Sales", "Visibility") & device == "Desktop" & strategy == "unknown", cost, 0),
           D_Sa_Mo_Pr = ifelse(campaign_type %in% c("Sales", "Visibility") & device == "Mobile" & strategy == "Prospecting", cost, 0),
           D_Sa_Mo_Re = ifelse(campaign_type %in% c("Sales", "Visibility") & device == "Mobile" & strategy == "Retargeting", cost, 0),
           D_Sa_Mo_un = ifelse(campaign_type %in% c("Sales", "Visibility") & device == "Mobile" & strategy == "unknown", cost, 0),
           D_Sa_Ta_Pr = ifelse(campaign_type %in% c("Sales", "Visibility") & device == "Tablet" & strategy == "Prospecting", cost, 0),
           D_Sa_Ta_Re = ifelse(campaign_type %in% c("Sales", "Visibility") & device == "Tablet" & strategy == "Retargeting", cost, 0),
           D_Sa_Ta_un = ifelse(campaign_type %in% c("Sales", "Visibility") & device == "Tablet" & strategy == "unknown", cost, 0),
           D_Sa_un_Pr = ifelse(campaign_type %in% c("Sales", "Visibility") & device == "unknown" & strategy == "Prospecting", cost, 0),
           D_Sa_un_Re = ifelse(campaign_type %in% c("Sales", "Visibility") & device == "unknown" & strategy == "Retargeting", cost, 0),
           D_Sa_un_un = ifelse(campaign_type %in% c("Sales", "Visibility") & device == "unknown" & strategy == "unknown", cost, 0),
           D_Me_De_Pr = ifelse(campaign_type == "Media" & device == "Desktop" & strategy == "Prospecting", cost, 0),
           D_Me_De_Re = ifelse(campaign_type == "Media" & device == "Desktop" & strategy == "Retargeting", cost, 0),
           D_Me_De_un = ifelse(campaign_type == "Media" & device == "Desktop" & strategy == "unknown", cost, 0),
           D_Me_Mo_Pr = ifelse(campaign_type == "Media" & device == "Mobile" & strategy == "Prospecting", cost, 0),
           D_Me_Mo_Re = ifelse(campaign_type == "Media" & device == "Mobile" & strategy == "Retargeting", cost, 0),
           D_Me_Mo_un = ifelse(campaign_type == "Media" & device == "Mobile" & strategy == "unknown", cost, 0),
           D_Me_Ta_Pr = ifelse(campaign_type == "Media" & device == "Tablet" & strategy == "Prospecting", cost, 0),
           D_Me_Ta_Re = ifelse(campaign_type == "Media" & device == "Tablet" & strategy == "Retargeting", cost, 0),
           D_Me_Ta_un = ifelse(campaign_type == "Media" & device == "Tablet" & strategy == "unknown", cost, 0),
           D_Me_un_Pr = ifelse(campaign_type == "Media" & device == "unknown" & strategy == "Prospecting", cost, 0),
           D_Me_un_Re = ifelse(campaign_type == "Media" & device == "unknown" & strategy == "Retargeting", cost, 0),
           D_Me_un_un = ifelse(campaign_type == "Media" & device == "unknown" & strategy == "unknown", cost, 0)) %>% 
    select(-subsidiary_id, -language_id, -channel, -buying_method, -campaign_type, -device, -strategy, -product_type_id, -cost, -product_type_name) %>% 
    group_by(date) %>% 
    summarise_all(sum) %>% 
    ungroup()

sales_perday <- SalesBE[order(SalesBE$date), c("date", "sales")]

#' Update modeldata: add display costs and sales  
modelData <- left_join(sales_perday, Display_per_combi, by = "date")

#' We take the total social costs on one day
Social_per_combi <- SocialBE %>% 
    mutate(S_Sa_Fa_Pr = ifelse(goal %in% c("Sales", "Visibility") & channel == "Facebook" & strategy == "Prospecting", cost, 0),
           S_Sa_Fa_Re = ifelse(goal %in% c("Sales", "Visibility") & channel == "Facebook" & strategy == "Retargeting", cost, 0),
           S_Sa_FI_Pr = ifelse(goal %in% c("Sales", "Visibility") & channel == "Facebook and Instagram" & strategy == "Prospecting", cost, 0),
           S_Sa_FI_Re = ifelse(goal %in% c("Sales", "Visibility") & channel == "Facebook and Instagram" & strategy == "Retargeting", cost, 0),
           S_Sa_In_Pr = ifelse(goal %in% c("Sales", "Visibility") & channel == "Instagram" & strategy == "Prospecting", cost, 0),
           S_Sa_In_Re = ifelse(goal %in% c("Sales", "Visibility") & channel == "Instagram" & strategy == "Retargeting", cost, 0),
           S_Me_Fa_Pr = ifelse(goal == "Media" & channel == "Facebook" & strategy == "Prospecting", cost, 0),
           S_Me_Fa_Re = ifelse(goal == "Media" & channel == "Facebook" & strategy == "Retargeting", cost, 0),
           S_Me_FI_Pr = ifelse(goal == "Media" & channel == "Facebook and Instagram" & strategy == "Prospecting", cost, 0),
           S_Me_FI_Re = ifelse(goal == "Media" & channel == "Facebook and Instagram" & strategy == "Retargeting", cost, 0),
           S_Me_In_Pr = ifelse(goal == "Media" & channel == "Instagram" & strategy == "Prospecting", cost, 0),
           S_Me_In_Re = ifelse(goal == "Media" & channel == "Instagram" & strategy == "Retargeting", cost, 0)) %>% 
    select(-subsidiaryid, -language_id, -channel, -goal, -device, -strategy, -product_type_id, -cost) %>% 
    group_by(date) %>% 
    summarise_all(sum) %>% 
    ungroup()

#' Update modeldata: add social costs
modelData <- left_join(modelData, Social_per_combi, by = "date")

#' Update modeldata: add search_ads costs and shopping_ads costs
adspervolume <- OtherMarketingBE$searchads_cost / OtherMarketingBE$search_volume
shoppingpervolume <- OtherMarketingBE$shoppingads_cost / OtherMarketingBE$search_volume
googleCosts <- data.frame(matrix(0,nrow(OtherMarketingBE),3))
names(googleCosts) <- cbind("date", "adspervolume", "shoppingpervolume")
googleCosts[,"date"] <- OtherMarketingBE$date
googleCosts[,"adspervolume"] <- adspervolume
googleCosts[, "shoppingpervolume"] <- shoppingpervolume
modelData <- left_join(modelData, googleCosts, by = "date")

modelData <- left_join(modelData,OtherMarketingBE[,c("date", "delivered_emails")], by = "date")

#' Update modeldata: add extrapolated competitor data
CompetitorBE <- Competitor %>% 
    filter(country == "Belgie NL") %>% 
    mutate(sellsPhones = ifelse(brand %in% c("ALIEXPRESS", "ALTERNATE", "AMAZON", "APPLE", "BCC", "BOL.COM", "EXPERT", "KREFEL", "LENOVO", "LG", "MEDIA MARKT", "MEDIAMARKT", "SAMSUNG", "VANDEN BORRE", "WEHKAMP.NL"), 1, 0),
           GRPsellsPhones = ifelse(sellsPhones == 1, GRP, 0),
           GRPnoPhones = ifelse(sellsPhones == 0, GRP, 0))

CompetitorBE <- CompetitorBE %>% 
    select(- c(channel, brand, country, sellsPhones)) %>% 
    group_by(date) %>% 
    summarise_all(sum)

datums <- SalesBE %>% 
    select(date)

CompetitorNA <- datums %>% 
    left_join(CompetitorBE, by = "date")

set.seed(6)
ExtrapolerensellsPhones <- mice(CompetitorNA %>% select(date, GRPsellsPhones), m = 1, method = "pmm", maxit = 1)
CompletedDatasellPhones <- mice::complete(ExtrapolerensellsPhones, 1)

set.seed(12)
ExtrapolerenNoPhones <- mice(CompetitorNA %>% select(date, GRPnoPhones), m = 1, method = "pmm", maxit = 1)
CompletedDataNoPhones <- mice::complete(ExtrapolerenNoPhones, 1)

CompetitorExtrapolated <- CompletedDatasellPhones %>% 
    left_join(CompletedDataNoPhones, by = "date")

modelData <- left_join(modelData, CompetitorExtrapolated, by = "date")

KNMI_data <- read.table("etmgeg_380.txt", header = TRUE, sep = ",", dec = ".")

#' Put dates from integer to date format and rename
dates <- as.character(KNMI_data$YYYYMMDD)
dates <- as.Date(dates, "%Y%m%d")
KNMI_data[,2] <- dates
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
priceDummies <- data.frame(matrix(0,nrow(OtherMarketingBE),3))
priceDummies <- OtherMarketingBE[, c(1, 14, 15)]
names(priceDummies) <- cbind("date", "cheapest", "cheaperthanavg")
priceDummies$cheapest <- priceDummies$cheapest - min(priceDummies$cheapest)
priceDummies$cheaperthanavg <- priceDummies$cheaperthanavg - min(priceDummies$cheaperthanavg)

modelData <- left_join(modelData, priceDummies, by = "date")

#' Update modeldata: add marketing costs televisions and laptops
SpillOverDisplay <- Display %>% 
    filter(subsidiary_id == 3,
           language_id != 4,
           product_type_id == c("2627", "17632")) %>% 
    group_by(date, product_type_id) %>% 
    summarise(costDisplay = sum(cost))

SpillOverSocial <- Social %>% 
    filter(subsidiaryid == 1,
           language_id != 4,
           product_type_id == c("2627", "17632")) %>% 
    group_by(date, product_type_id) %>% 
    summarise(costSocial = sum(cost))

SpillOverOther <- OtherMarketing %>% 
    filter(key %in% c("17632_31", "2627_31")) %>% 
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
    filter(key %in% c("17632_31", "2093_31", "2627_31", "2675_31", "2676_31", "2677_31", "2678_31", "2679_31", "8804_31")) %>% 
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
    left_join(FeestdagenBE, by = "date")

#' Remove data from environment that is now in one big table
rm(sales_perday, Display_per_combi, Social_per_combi, priceDummies, CompetitorNA, CompetitorExtrapolated, ExtrapolerensellsPhones, ExtrapolerenNoPhones, CompletedDatasellPhones, CompletedDataNoPhones, SpillOver, SpillOverOther, SpillOverSocial, SpillOverDisplay)

modelData[is.na(modelData)] <- 0
